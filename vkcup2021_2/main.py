import os
import argparse

from lib import (
    Data,
    LgbModel,
)

def main():
    parser = argparse.ArgumentParser(description='Program.')
    parser.add_argument('--test-data-dir', dest='test_data_dir', required=True)
    parser.add_argument('--train-data-dir', dest='train_data_dir', required=True)
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
    train = Data()
    train.read(args.train_data_dir, 'train')

    test = Data()
    test.read(args.test_data_dir, 'test')

    model.prepare(train, test)

    model.load(os.path.join(args.work_dir, 'models/lgb.txt'))

    res = model.predict(test)
    res.rename(columns={'res': 'age'}).to_csv(args.res_path, header=True, index=False)


if __name__ == '__main__':
    main()
