import argparse
import pandas as pd
import numpy as np

from lib import (
    Data,
    LgbModel,
    KerasModel,
    MeanModel,
    seed_everything,
    DebugError,
)


def main():
    parser = argparse.ArgumentParser(description='Program.')
    parser.add_argument('--test-data-dir', dest='test_data_dir', required=True)
    parser.add_argument('--train-data-dir', dest='train_data_dir', required=True)
    parser.add_argument('--work-dir', dest='work_dir', required=True)
    parser.add_argument('--res-path', dest='res_path', required=True)
    args = parser.parse_args()

    print(args)
    seed_everything(2020)

    prod_model = MeanModel([
        LgbModel({
            'boosting_type': 'gbdt',
            'min_data_in_leaf': 25,
            'lambda_l2': 0.06,
            'num_leaves': 20,
            'learning_rate': 0.035,
            'feature_fraction': 0.6,
            'bagging_fraction': 0.9,
            'bagging_freq': 6,
            'num_boost_round': 400,
            'group_embeddings_n_components': 7,
            'group_embeddings_n_iter': 24,
        }),
        KerasModel({
            'epochs': 100,
            'verbose': 0,
            'n1': 100,
            'n2': 70,
            'dropout': 0.1,
            'learning_rate': 0.003,
        }),
    ], coefs=(0.7, 0.3))

    train = Data()
    train.read(args.train_data_dir, 'train')

    test = Data()
    test.read(args.test_data_dir, 'test')

    prod_model.prepare(train, test)
    prod_model.fit(train)

    #prod_model.load(os.path.join(args.work_dir, 'models/lgb.txt'))

    try:
        res = prod_model.predict(test)
        pd.DataFrame({'uid': test.edu.uid, 'age': res}).to_csv(args.res_path, header=True, index=False)
    except DebugError:
        pd.DataFrame({'uid': test.edu.uid, 'age': np.repeat(35, test.edu.shape[0])}).to_csv(args.res_path, header=True, index=False)


if __name__ == '__main__':
    main()
