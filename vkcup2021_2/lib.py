import datetime
import os

from collections import defaultdict

import lightgbm as lgb
import pandas as pd
import numpy as np


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
        result.groups = self.groups  # TODO
        return result


class MyModel:
    verbose = 0
    registered_year_by_uid = None
    age_by_uid = None

    def __init__(self, params):
        self.params = params or {}

    def prepare(self, train, test):
        self.registered_year_by_uid = {}
        self.age_by_uid = {}
        for row in train.edu.itertuples():
            self.registered_year_by_uid[row.uid] = row.registered_year
            self.age_by_uid[row.uid] = row.age

    def get_X(self, data):
        del_cols = ['age']
        res = data.edu.drop([c for c in del_cols if c in data.edu.columns], 1)
        uids = res['uid'].values
        res['friends_count'] = [len(data.friends.get(uid, [])) for uid in uids]
        res['groups_count'] = [len(data.groups.get(uid, [])) for uid in uids]
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
dummy_model = MyModel(None)
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
