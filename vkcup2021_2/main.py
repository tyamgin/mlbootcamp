import os
import math
import argparse
import pandas as pd


def solve(edu):
    edu['res'] = 34
    dct = {
        'school_education': 17,  # 4.903859550418915
        'graduation_1': 25,  # 5.590608371258384
        'graduation_2': 24,  # 4.912185675064848
        'graduation_3': 24,  # 4.905354217587146
        'graduation_4': 25,  # 4.009603855360661
        'graduation_5': 24,  # 5.55578449198568
        'graduation_6': 25,  # 5.225676253624243
        'graduation_7': 25,  # 7.671183665089072
    }
    for grad, shift in dct.items():
        grad_idx = ~edu[grad].isna()
        edu.loc[grad_idx, 'res'] = 2021 - edu.loc[grad_idx, grad] + shift

    return edu[['uid', 'res']].set_index('uid')


def main():
    parser = argparse.ArgumentParser(description='Program.')
    parser.add_argument('--data-dir', dest='data_dir', required=True)
    parser.add_argument('--res-path', dest='res_path', required=True)
    args = parser.parse_args()

    print(args)
    test_edu = pd.read_csv(os.path.join(args.data_dir, 'testEducationFeatures.csv'))
    res = solve(test_edu)
    res.rename(columns={'res': 'age'}).to_csv(args.res_path, header=True)


if __name__ == '__main__':
    main()
