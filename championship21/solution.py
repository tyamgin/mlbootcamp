import pandas as pd
import lightgbm as lgb


def min2(a):
    if hasattr(a, 'values'):
        a = a.values
    if len(a) <= 1:
        return a[0]
    return sorted(a)[1]


class MyModel:
    def __init__(self, params=None):
        self.params = params or {}

    def fit(self, train, train_answers):
        pass

    def predict(self, X):
        return None


class LgbModel(MyModel):
    def get_x(self, data):
        return data[self.params['feats']]

    def fit(self, data, answers=None):
        lgb_train = lgb.Dataset(self.get_x(data), data['target'])
        params = self.params.copy()
        num_boost_round = params['num_boost_round']
        del params['num_boost_round']
        del params['feats']
        params['objective'] = 'fair'
        self.model = lgb.train(params, lgb_train, num_boost_round=num_boost_round)

    def predict(self, data):
        return self.model.predict(self.get_x(data))


class MinMaxModelEx(MyModel):
    def create_df(self, df, answers, targeter=None):
        feats = df.copy()
        feats['width'] = df.Xmax - df.Xmin
        feats['height'] = df.Ymax - df.Ymin
        feats['ratio'] = ((feats.width - feats.height) / (feats.width + feats.height)).fillna(1)

        feats = feats.join(df.groupby('itemId').agg({'userId': 'count'}).rename(columns={'userId': 'usersCount'}), on='itemId')

        res = feats.groupby('itemId').apply(lambda a: pd.Series({
            'width': a.width.mean(),
            'usersCount': a.usersCount.mean(),
            'Xmin_min': a.Xmin.min(),
            'Xmin_min2': min2(a.Xmin),
            'Ymin_min': a.Ymin.min(),
            'Xmax_max': a.Xmax.max(),
            'Ymax_max': a.Ymax.max(),
            'ratio_mean': a.ratio.mean(),
        }))
        res.reset_index(inplace=True)

        if answers is not None:
            res = res.merge(answers, on='itemId')
            res['target'] = targeter(res)
            res.drop(['Xmin_true', 'Ymin_true', 'Xmax_true', 'Ymax_true'], 1, inplace=True)
        return res

    def fit(self, train, train_answers):
        self.x1_targeter = lambda df: df.Xmin_min - df.Xmin_true  # сколько нужно отнять от Xmin
        self.y2_targeter = lambda df: df.Ymax_max - df.Ymax_true  # сколько нужно отнять от Ymax

        feats_x1 = self.create_df(train, train_answers, targeter=self.x1_targeter)
        feats_y2 = self.create_df(train, train_answers, targeter=self.y2_targeter)

        lgb_params = {
            'boosting_type': 'gbdt',
            'min_data_in_leaf': 100,
            'lambda_l2': 0.5,
            'num_leaves': 5,
            'learning_rate': 0.007,
            'feature_fraction': 1,
            'bagging_fraction': 1,
            'bagging_freq': 1,
            'num_boost_round': 700,
            'verbose': 0,
            'feats': None,
        }

        self.model_x1 = LgbModel(dict(lgb_params, feats=['Xmin_min']))
        self.model_x1.fit(feats_x1)

        self.model_y2 = LgbModel(dict(lgb_params, feats=['Xmin_min', 'Xmin_min2', 'width', 'usersCount', 'Ymin_min', 'ratio_mean']))
        self.model_y2.fit(feats_y2)

    def do_magic(self, x1_orig, x1, y1, x2, y2):
        """
        Тут основная часть решения.
        Коэффициенты и операции по сдвигу x1,y1,x2,y2 подбирались вручную методом случайных правок кода,
        после чего некоторые коэффициенты корректировались генетикой.

        Может показаться, что это переобучение, но подгон коэффициентов тоже проходил полноценную кросс-валидацию.
        После чего подгон запускался на всём трейне, и коэффициенты вставлялись сюда.
        """

        mult_dx = 0.7415657456058256
        mult_dd = 0.5640671461647451
        thr1 = 80.04833508987377
        thr2 = 17.020502776535906
        thr3 = 14.801316697593093
        c1 = 0.06141877165368582
        c2 = 0.36311996949549297
        p2 = 28.753285758777665
        mult_dd2 = 0.3
        p1 = 25
        p3 = 12
        p4 = 5
        p5 = 10
        mult_dx2 = 1.2 / 60
        mult_dd3 = 0.1

        if x1_orig >= 70:
            x1 -= x2 * mult_dx2

        """
        Отклонениие от среднего ratio. Как в меньшую сторону, так и в большую
        под ratio подразумевается разность ширины (с коэффициентом mult_dx) и высоты
        """
        dx = (x2 - x1) * mult_dx
        dy = y2 - y1
        if dy > dx:
            dd = (dy - dx)
            y2 -= dd * mult_dd
            if x1 >= thr1:
                x1 -= dd * c1
            else:
                x2 += dd * c2
        else:
            dd = (dx - dy)
            y2 += dd * mult_dd2
            y1 -= dd * mult_dd3

        """
        Отклонение слишком мелких y по отношению к x
        """
        if y2 < x1 * thr3:
            y2 += ((x1 * thr3 - y2) / (x1 * thr3)) * p1
            y1 -= ((x1 * thr3 - y2) / (x1 * thr3)) * p3

        if y1 < x1 * thr2:
            x1 -= ((x1 * thr2 - y1) / (x1 * thr2)) * p2

        """
        Отклонение от средней высоты большую сторону
        """
        if dy > 318:
            y2 -= (dy - 318) / 318 * p4

        """
        Отклонение от средней ширины в большую сторону
        """
        v = 450
        if dx / mult_dx > v:
            x2 -= (dx / mult_dx - v) / v * p5
            x1 += (dx / mult_dx - v) / v * p5

        return x1, y1, x2, y2

    def predict(self, X):
        feats_x1 = self.create_df(X, None, targeter=self.x1_targeter)
        feats_y2 = self.create_df(X, None, targeter=self.y2_targeter)

        pred = pd.DataFrame({
            'itemId': feats_x1.itemId,
            'target_x1': self.model_x1.predict(feats_x1),
            'target_y2': self.model_y2.predict(feats_y2),
        })

        result = []
        for items in feats_x1.join(pred.set_index('itemId'), on='itemId').itertuples():
            x1 = items.Xmin_min
            y1 = items.Ymin_min
            x2 = items.Xmax_max
            y2 = items.Ymax_max
            x1_orig = x1

            if x1_orig >= 70:
                x1 -= items.target_x1

            y2 -= items.target_y2 * 0.1

            x1, y1, x2, y2 = self.do_magic(x1_orig, x1, y1, x2, y2)

            result.append([items.itemId, x1, y1, x2, y2])

        return pd.DataFrame(result, columns=['itemId', 'Xmin', 'Ymin', 'Xmax', 'Ymax'])


train = pd.read_csv('data/train_data.csv', sep=',')
test = pd.read_csv('data/test_data.csv', sep=',')
train_answers = pd.read_csv('data/train_answers.csv', sep=',')

model = MinMaxModelEx()
model.fit(train, train_answers)
res_test = model.predict(test).sort_values(by='itemId')
print(res_test.head(10))

with open('res/res.txt', 'w') as out:
    lines = res_test.apply(lambda x: '%d,%f,%f,%f,%f' % (x.itemId, x.Xmin, x.Ymin, x.Xmax, x.Ymax), axis=1).values
    out.write('\n'.join(lines) + '\n')
