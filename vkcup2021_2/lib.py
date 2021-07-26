import datetime
import os

import lightgbm as lgb
import pandas as pd
import numpy as np


class Data:
    friends = None
    edu = None
    groups = None

    def read(self, data_folder, prefix):
        self.friends = pd.read_csv(os.path.join(data_folder, 'friends.csv'))
        data = pd.read_csv(os.path.join(data_folder, f'{prefix}.csv'))
        self.edu = pd.read_csv(os.path.join(data_folder, f'{prefix}EducationFeatures.csv')).set_index('uid').join(
            data.set_index('uid')).reset_index()
        self.groups = pd.read_csv(os.path.join(data_folder, f'{prefix}Groups.csv'))

    def get(self, idxes):
        result = Data()
        result.friends = self.friends
        result.edu = self.edu[self.edu['uid'].isin(idxes)]
        result.groups = self.groups  # TODO
        return result


class MyModel:
    verbose = 0

    def __init__(self, params=None):
        self.params = params or {}

    def get_X(self, data):
        del_cols = ['age']
        return data.edu.drop([c for c in del_cols if c in data.edu.columns], 1)

    def fit(self, data):
        pass

    def predict(self, edu):
        return np.repeat(35, edu.shape[0])

dummy_model = MyModel(None)


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
