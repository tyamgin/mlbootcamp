import itertools
import datetime
import math
import os

from collections import defaultdict

import lightgbm as lgb
import pandas as pd
import numpy as np
import scipy.stats
import tensorflow.keras as keras

from sklearn.decomposition import TruncatedSVD
from scipy.sparse import coo_matrix


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
    registered_year_by_uid = None
    school_education_by_uid = None
    age_by_uid = None
    group_median_age = None
    group_size = None
    group_median_registered_year = None
    group_max_registered_year = None
    group_embeddings = None

    def __init__(self, params):
        self.params = params or {}

    def prepare(self, train, test):
        self.registered_year_by_uid = {}
        self.school_education_by_uid = {}
        self.age_by_uid = {}
        self.group_median_age = {}
        self.group_size = defaultdict(int)
        self.group_median_registered_year = {}
        self.group_max_registered_year = {}
        group_users = defaultdict(list)
        num_trains = 0

        x = []
        y = []
        uids_list = []
        num_rows = 0

        for dataset in (train, test):
            if dataset is None:
                continue
            is_train = 'age' in dataset.edu.columns
            num_trains += int(is_train)
            for row in dataset.edu.itertuples():
                self.registered_year_by_uid[row.uid] = row.registered_year
                self.school_education_by_uid[row.uid] = row.school_education
                if is_train:
                    self.age_by_uid[row.uid] = row.age
                for gid in dataset.groups.get(row.uid, []):
                    x.append(num_rows)
                    y.append(gid)
                uids_list.append(row.uid)
                num_rows += 1

            for uid, user_groups in dataset.groups.items():
                for gid in user_groups:
                    group_users[gid].append(uid)

        for gid, uids in group_users.items():
            self.group_median_age[gid] = np.median([
                self.age_by_uid[uid]
                for uid in uids
                if uid in self.age_by_uid
            ])
            self.group_median_registered_year[gid] = np.median([
                self.registered_year_by_uid[uid]
                for uid in uids
            ])
            self.group_max_registered_year[gid] = np.max([
                self.registered_year_by_uid[uid]
                for uid in uids
            ])
            self.group_size[gid] = len(uids)

        assert num_trains == 1

        mat = coo_matrix((np.repeat(1, len(x)), (x, y)), shape=(num_rows, max(y) + 1))
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
        #res['dff'] = res['registered_year'] - res['school_education']
        res['friends_median_registered_year'] = [
            np.median([
                self.registered_year_by_uid[fr]
                for fr in data.friends.get(uid, [])
                if fr in self.registered_year_by_uid
            ])
            for uid in uids
        ]
        res['friends_median_age'] = [
            np.median([
                self.age_by_uid[fr]
                for fr in data.friends.get(uid, [])
                if fr in self.age_by_uid
            ])
            for uid in uids
        ]
        # res['friends_mode_age'] = [
        #     array_mode([
        #         self.age_by_uid[fr]
        #         for fr in data.friends.get(uid, [])
        #         if fr in self.age_by_uid
        #     ])
        #     for uid in uids
        # ]
        res['friends_mean_age'] = [
            np.mean([
                self.age_by_uid[fr]
                for fr in data.friends.get(uid, [])
                if fr in self.age_by_uid
            ])
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
        # res['friends_mean_school_education_isna'] = [
        #     np.mean([
        #         np.isnan(self.school_education_by_uid[fr])
        #         for fr in data.friends.get(uid, [])
        #         if fr in self.school_education_by_uid
        #     ])
        #     for uid in uids
        # ]

        # res['groups_median_max_registered_year'] = [
        #     np.nanmedian([
        #         self.group_max_registered_year[gr]
        #         for gr in data.groups.get(uid, [])
        #         if gr in self.group_max_registered_year
        #     ])
        #     for uid in uids
        # ]
        # res['groups_median_size'] = [
        #     np.median([
        #         self.group_size[gr]
        #         for gr in data.groups.get(uid, [])
        #         if gr in self.group_size
        #     ])
        #     for uid in uids
        # ]

        # TODO: группа, в которой давно не было новичков:
        # reg_year - max(reg_year)

        res = res.set_index('uid').join(self.group_embeddings.set_index('uid')).reset_index()

        return res

    def fit(self, X):
        pass

    def predict(self, edu):
        return np.repeat(35, edu.shape[0])

dummy_data = Data()
dummy_data.edu = pd.DataFrame({
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
dummy_data.groups = {
    356: np.arange(0, 1000)
}
dummy_model = MyModel({
    'group_embeddings_n_components': 5,
    'group_embeddings_n_iter': 40,
})
dummy_model.prepare(dummy_data, None)
dummy_model.fit(dummy_data)

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
        del params['num_boost_round']
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
            if c != 'label':
                self.columns.append(c)

        self.means = []
        self.sds = []
        self.mins = []
        self.maxs = []
        for c in self.columns:
            self.means.append(np.mean(X[c]))
            self.sds.append(np.std(X[c]))
            self.mins.append(np.min(X[c]))
            self.maxs.append(np.max(X[c]))
        return self.transform(X, inplace=inplace)

    def transform(self, X, inplace=False):
        assert inplace
        for c, mean, sd, mn, mx in zip(self.columns, self.means, self.sds, self.mins, self.maxs):
            X[c] = ((X[c] - mean) / sd).astype(np.float32)
            # X[c] = ((X[c].astype(np.float64) - float(mn)) / (float(mx) - float(mn))).astype(np.float32)

class KerasModel(MyModel):
    model = None

    def fit(self, data):
        params = self.params
        self.scaler = MyScaler()
        X = self.get_X(data)
        self.scaler.fit_transform(X, inplace=True)
        y = data.edu['age'].values

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
        model.summary()
        model.compile(
            optimizer=keras.optimizers.Adam(lr=0.001, beta_1=0.9, beta_2=0.999, epsilon=None, decay=0.0, amsgrad=False),
            loss="binary_crossentropy",
            metrics=["accuracy"]
        )
        results = model.fit(
            X, y,
            epochs=params['epochs'],
            batch_size=params.get('batch_size', 1024),
            verbose=params['verbose']
        )

    def predict(self, data):
        res = self.model.predict(self.get_X(data))
        return pd.DataFrame({'uid': data.edu['uid'].values, 'res': res})

    def load(self, model_file):
        self.model = lgb.Booster(model_file=model_file)
