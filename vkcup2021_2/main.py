import os
import math
import argparse
import pandas as pd


def main():
    parser = argparse.ArgumentParser(description='Program.')
    parser.add_argument('--data-dir', dest='data_dir', required=True)
    parser.add_argument('--res-path', dest='res_path', required=True)
    args = parser.parse_args()

    print(args)
    test_edu = pd.read_csv(os.path.join(args.data_dir, 'testEducationFeatures.csv'))
    test_edu['res'] = 35
    test_edu[['uid', 'res']].set_index('uid').rename(columns={'res': 'age'}).to_csv(args.res_path, header=True)


if __name__ == '__main__':
    main()
