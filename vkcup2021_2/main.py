import argparse

from lib import (
    Data,
    LgbModel,
    KerasModel,
    MeanModel,
    seed_everything,
)


def main():
    parser = argparse.ArgumentParser(description='Program.')
    parser.add_argument('--test-data-dir', dest='test_data_dir', required=True)
    parser.add_argument('--train-data-dir', dest='train_data_dir', required=True)
    parser.add_argument('--work-dir', dest='work_dir', required=True)
    parser.add_argument('--res-path', dest='res_path', required=True)
    args = parser.parse_args()

    print(args)
    seed_everything(2021)

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
            'group_embeddings_n_components': 5,
            'group_embeddings_n_iter': 44,
        }),
        KerasModel({
            'epochs': 100,
            'verbose': 0,
            'n1': 170,
            'n2': 150,
            'dropout': 0.1,
            'learning_rate': 0.001,
        }),
    ], coefs=(0.7, 0.3))

    train = Data()
    train.read(args.train_data_dir, 'train')

    test = Data()
    test.read(args.test_data_dir, 'test')

    prod_model.prepare(train, test)
    prod_model.fit(train)

    #prod_model.load(os.path.join(args.work_dir, 'models/lgb.txt'))

    res = prod_model.predict(test)
    res.rename(columns={'res': 'age'}).to_csv(args.res_path, header=True, index=False)


if __name__ == '__main__':
    main()
