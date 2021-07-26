import os
import math
import argparse
import pandas as pd

from lib import (
    Data,
    LgbModel,
)

def main():
    parser = argparse.ArgumentParser(description='Program.')
    parser.add_argument('--data-dir', dest='data_dir', required=True)
    parser.add_argument('--work-dir', dest='work_dir', required=True)
    parser.add_argument('--res-path', dest='res_path', required=True)
    args = parser.parse_args()

    print(args)

    model = LgbModel({
        'boosting_type': 'gbdt',
        'min_data_in_leaf': 25,
        'lambda_l2': 0.0,
        'num_leaves': 10,
        'learning_rate': 0.03,
        'feature_fraction': 1,
        'bagging_fraction': 1,
        'bagging_freq': 5,
        'num_boost_round': 200,
    })
    model.load(os.path.join(args.work_dir, 'models/lgb.txt'))

    test = Data()
    test.read(args.data_dir, 'test')

    res = model.predict(test)
    res.rename(columns={'res': 'age'}).to_csv(args.res_path, header=True, index=False)


if __name__ == '__main__':
    main()
