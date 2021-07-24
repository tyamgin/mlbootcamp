import os
import math
import argparse
import pandas as pd


def solve(edu):
    edu['res'] = 35
    school_idx = ~edu.school_education.isna()
    edu.loc[school_idx, 'res'] = 2021 - edu[school_idx].school_education + 17
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
