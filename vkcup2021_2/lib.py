import itertools
import datetime
import random
import math
import os

from collections import defaultdict

import lightgbm as lgb
import pandas as pd
import numpy as np
import scipy.stats
import tensorflow
import tensorflow.keras as keras

from sklearn.decomposition import TruncatedSVD
from scipy.sparse import coo_matrix


def seed_everything(seed):
    random.seed(seed)
    os.environ['PYTHONHASHSEED'] = str(seed)
    np.random.seed(seed)
    #torch.manual_seed(seed)
    #torch.cuda.manual_seed(seed)
    #torch.backends.cudnn.deterministic = True
    tensorflow.random.set_seed(seed)


def expand_grid(dictionary):
    return pd.DataFrame([row for row in itertools.product(*dictionary.values())], columns=dictionary.keys())


def rmse(x, y):
    n = len(x)
    return math.sqrt(1.0 * sum((x - y)**2) / n)


def array_mode(x):
    mode = scipy.stats.mode(x)
    if len(mode[0]) == 0:
        return np.nan
    if mode.count[0] < len(x) / 5:
        return np.median(x)
    return mode[0][0]


class Data:
    friends = None
    edu = None
    groups = None

    def _prepare_friends(self, csv):
        res = defaultdict(list)
        for row in csv.itertuples():
            res[row.uid].append(row.fuid)
            res[row.fuid].append(row.uid)
        return {uid: np.array(sorted(lst)) for uid, lst in res.items()}

    def _prepare_groups(self, csv):
        res = defaultdict(list)
        for row in csv.itertuples():
            res[row.uid].append(row.gid)
        return {uid: np.array(sorted(lst)) for uid, lst in res.items()}

    def read(self, data_folder, prefix):
        self.friends = self._prepare_friends(pd.read_csv(os.path.join(data_folder, 'friends.csv')))
        data = pd.read_csv(os.path.join(data_folder, f'{prefix}.csv'))
        self.edu = pd.read_csv(os.path.join(data_folder, f'{prefix}EducationFeatures.csv')).set_index('uid').join(
            data.set_index('uid')).reset_index()
        self.groups = self._prepare_groups(pd.read_csv(os.path.join(data_folder, f'{prefix}Groups.csv')))

    def get(self, idxes):
        result = Data()
        result.friends = self.friends
        result.edu = self.edu[self.edu['uid'].isin(idxes)]
        gset = set(result.edu.uid)
        result.groups = {uid: user_groups
                         for uid, user_groups in self.groups.items()
                         if uid in gset}
        return result


class MyModel:
    verbose = 0
    train_size = None
    registered_year_by_uid2 = None
    age_by_uid2 = None
    group_median_age = None
    group_size = None
    group_median_registered_year = None
    group_embeddings = None

    def __init__(self, params):
        self.params = params or {}

    def prepare(self, train, test):
        num_trains = 0
        num_uids = 0
        self.uid2idx = {}
        for dataset in (train, test):
            is_train = 'age' in dataset.edu.columns
            num_trains += int(is_train)
            for row in dataset.edu.itertuples():
                self.uid2idx[row.uid] = num_uids
                num_uids += 1

        assert num_trains == 1
        self.train_size = train.edu.shape[0]

        self.group_median_age = {}
        self.group_size = defaultdict(int)
        self.group_median_registered_year = {}

        test_edu = test.edu.copy()
        test_edu['age'] = np.nan
        edu = pd.concat((train.edu, test_edu), sort=False).reset_index()
        groups = {**train.groups, **test.groups}

        self.registered_year_by_uid2 = edu['registered_year'].values
        self.age_by_uid2 = edu['age'].values

        x = []
        y = []
        uids_list = []

        for i, row in edu.iterrows():
            is_train = not np.isnan(row.age)
            for gid in (train.groups.get(row.uid, []) if is_train else test.groups.get(row.uid, [])):
                x.append(i)
                y.append(gid)
            uids_list.append(row.uid)

        group_users2 = defaultdict(list)
        for uid, user_groups in groups.items():
            idx = self.uid2idx[uid]
            for gid in user_groups:
                group_users2[gid].append(idx)

        for gid, uids in group_users2.items():
            self.group_size[gid] = len(uids)

        for gid, uidxs in group_users2.items():
            self.group_median_registered_year[gid] = np.median(self.registered_year_by_uid2[uidxs])
            self.group_median_age[gid] = np.nanmedian(self.age_by_uid2[uidxs])

        mat = coo_matrix((np.repeat(1, len(x)), (x, y)), shape=(len(uids_list), max(y) + 1))
        if self.params.get('group_embeddings_n_components', 0) > 0:
            svder = TruncatedSVD(
                n_components=self.params['group_embeddings_n_components'],
                n_iter=self.params['group_embeddings_n_iter']
            )
            feats = svder.fit_transform(mat)
            self.group_embeddings = pd.DataFrame(feats, columns=[f"group_emb_{i}" for i in range(feats.shape[1])])
            self.group_embeddings['uid'] = uids_list

    def get_X(self, data):
        del_cols = ['age']
        res = data.edu.drop([c for c in del_cols if c in data.edu.columns], 1)
        uids = res['uid'].values

        res['friends_count'] = [len(data.friends.get(uid, [])) for uid in uids]
        res['groups_count'] = [len(data.groups.get(uid, [])) for uid in uids]

        friends2 = {
            uid: [self.uid2idx[fr] for fr in uids_list if fr in self.uid2idx]
            for uid, uids_list in data.friends.items()
        }

        res['friends_median_registered_year'] = [
            np.nanmedian(self.registered_year_by_uid2[friends2.get(uid, [])])
            for uid in uids
        ]
        res['friends_median_age'] = [
            np.nanmedian(self.age_by_uid2[friends2.get(uid, [])])
            for uid in uids
        ]
        res['friends_mean_age'] = [
            np.nanmean(self.age_by_uid2[friends2.get(uid, [])])
            for uid in uids
        ]
        res['groups_median_age'] = [
            np.nanmedian([
                self.group_median_age[gr]
                for gr in data.groups.get(uid, [])
                if gr in self.group_median_age
            ])
            for uid in uids
        ]
        res['groups_median_registered_year'] = [
            np.nanmedian([
                self.group_median_registered_year[gr]
                for gr in data.groups.get(uid, [])
                if gr in self.group_median_registered_year
            ])
            for uid in uids
        ]
        res['friends_median_groups_count'] = [
            np.median([
                len(data.friends.get(fr, []))
                for fr in data.friends.get(uid, [])
            ])
            for uid in uids
        ]

        # TODO: группа, в которой давно не было новичков:
        # reg_year - max(reg_year)

        if self.group_embeddings is not None:
            res = res.set_index('uid').join(self.group_embeddings.set_index('uid')).reset_index()

        return res

    def fit(self, X):
        pass

    def predict(self, edu):
        return np.repeat(35, edu.shape[0])

dummy_train = Data()
dummy_test = Data()
dummy_train.edu = pd.DataFrame({
    'uid': [356],
    'school_education': [2010],
    'graduation_1': [2010],
    'graduation_2': [2010],
    'graduation_3': [2010],
    'graduation_4': [2010],
    'graduation_5': [2010],
    'graduation_6': [2010],
    'graduation_7': [2010],
    'age': [27],
    'registered_year': [2015],
})
dummy_train.groups = {
    356: np.arange(0, 1000)
}
dummy_test.edu = pd.DataFrame({
    'uid': [556],
    'school_education': [2010],
    'graduation_1': [2010],
    'graduation_2': [2010],
    'graduation_3': [2010],
    'graduation_4': [2010],
    'graduation_5': [2010],
    'graduation_6': [2010],
    'graduation_7': [2010],
    'registered_year': [2015],
})
dummy_test.groups = {
    556: np.arange(0, 1000)
}
dummy_model = MyModel({
    'group_embeddings_n_components': 5,
    'group_embeddings_n_iter': 40,
})
dummy_model.prepare(dummy_train, dummy_test)
dummy_model.fit(dummy_train)


class MeanModel(MyModel):
    def __init__(self, models, coefs):
        self.models = models
        self.coefs = coefs

    def fit(self, data):
        for model, coef in zip(self.models, self.coefs):
            if coef != 0:
                model.fit(data)

    def predict(self, X):
        sum = None
        for model, coef in zip(self.models, self.coefs):
            if coef != 0:
                r = model.predict(X) * coef
                if sum is None:
                    sum = r
                else:
                    sum += r
        return sum

    def prepare(self, train, test):
        for model, coef in zip(self.models, self.coefs):
            if coef != 0:
                model.prepare(train, test)


class LgbModel(MyModel):
    model = None

    def fit(self, data):
        y = data.edu['age']
        lgb_train = lgb.Dataset(self.get_X(data), y)
        categorical_feature = []

        # if self.verbose >= 2:
        print('Starting train: %s' % datetime.datetime.now())
        params = self.params.copy()
        num_boost_round = params['num_boost_round']
        for del_param in ('num_boost_round', 'group_embeddings_n_iter', 'group_embeddings_n_components'):
            if del_param in params:
                del params[del_param]

        params['objective'] = 'regression'
        params['metric'] = 'rmse'
        params['verbosity'] = 1
        self.model = lgb.train(
            params,
            lgb_train,
            num_boost_round=num_boost_round
        )

    def predict(self, data):
        res = self.model.predict(self.get_X(data))
        return pd.DataFrame({'uid': data.edu['uid'].values, 'res': res})

    def load(self, model_file):
        self.model = lgb.Booster(model_file=model_file)


class MyScaler():
    def fit_transform(self, X, inplace=False):
        assert inplace

        self.columns = []
        for c in X.columns:
            self.columns.append(c)

        self.means = []
        self.sds = []
        self.mins = []
        self.maxs = []
        self.defaults = []
        for c in self.columns:
            self.means.append(np.nanmean(X[c]))
            self.sds.append(np.nanstd(X[c]))
            self.mins.append(np.nanmin(X[c]))
            self.maxs.append(np.nanmax(X[c]))
            self.defaults.append(np.nanmean(X[c]))
        return self.transform(X, inplace=inplace)

    def transform(self, X, inplace=False):
        assert inplace
        for c, mean, sd, mn, mx, default in zip(self.columns, self.means, self.sds, self.mins, self.maxs, self.defaults):
            X[c] = ((X[c].fillna(default) - mean) / sd).astype(np.float32)
            # X[c] = ((X[c].astype(np.float64) - float(mn)) / (float(mx) - float(mn))).astype(np.float32)

class KerasModel(MyModel):
    model = None

    def fit(self, data):
        params = self.params
        self.scaler = MyScaler()
        X = self.get_X(data)
        self.scaler.fit_transform(X, inplace=True)
        y = data.edu['age'].values / 70

        model = keras.models.Sequential()
        self.model = model

        model.add(keras.layers.Dense(params['n1'], activation="relu", input_shape=(X.shape[1],)))
        model.add(keras.layers.BatchNormalization())
        model.add(keras.layers.LeakyReLU())

        model.add(keras.layers.Dropout(params['dropout'], noise_shape=None, seed=1))
        model.add(keras.layers.Dense(params['n2'], activation="relu"))
        model.add(keras.layers.BatchNormalization())

        if 'n3' in params:
            model.add(keras.layers.Dropout(params['dropout'], noise_shape=None, seed=1))
            model.add(keras.layers.Dense(params['n3'], activation="relu"))
            model.add(keras.layers.BatchNormalization())

        model.add(keras.layers.Dense(1, activation="sigmoid"))
        if params['verbose'] > 0:
            model.summary()
        model.compile(
            optimizer=keras.optimizers.Adam(learning_rate=0.001, beta_1=0.9, beta_2=0.999, epsilon=None, decay=0.0, amsgrad=False),
            loss=keras.metrics.mean_squared_error,
            metrics=[keras.metrics.RootMeanSquaredError(name='rmse')],
        )
        results = model.fit(
            X, y,
            epochs=params['epochs'],
            batch_size=params.get('batch_size', 1024),
            verbose=params['verbose']
        )

    def predict(self, data):
        X = self.get_X(data)
        self.scaler.transform(X, inplace=True)
        return pd.DataFrame({'uid': data.edu['uid'].values, 'res': self.model.predict(X)[:, 0] * 70})
