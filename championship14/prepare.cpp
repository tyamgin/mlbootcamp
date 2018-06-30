#define _CRT_SECURE_NO_WARNINGS
#pragma comment(linker, "/STACK:500000000") 
#include <functional>
#include <algorithm>
#include <iostream> 
#include <string.h> 
#include <stdlib.h>
#include <complex>
#include <sstream> 
#include <fstream>
#include <numeric>
#include <ctype.h> 
#include <stdio.h> 
#include <bitset>
#include <vector> 
#include <string> 
#include <math.h> 
#include <time.h> 
#include <queue> 
#include <stack> 
#include <list>
#include <map> 
#include <set> 
#define Int long long 
#define INF 0x3F3F3F3F 
#define eps 1e-9
using namespace std;

void processJson(const char* str, FILE* out, int idx)
{
	auto start = str;
	while(1)
	{
		while (*str != '}' && *str != 0 && !isdigit(*str))
			*str++;

		if (*str == 0)
		{
			cerr << "bad end of string" << endl;
			exit(1);
		}
		if (*str == '}')
			break;
		
		int key = 0;
		while (isdigit(*str))
			key = key * 10 + (*str++ - '0');
		while (!isdigit(*str))
			str++;
		int value = 0;
		while (isdigit(*str))
			value = value * 10 + (*str++ - '0');
		fprintf(out, "%d\t%d\t%d\n", key, value, idx);
	}
}

int main()
{
	string path = "G:\\Projects\\mlbootcamp\\championship14\\data\\";
	auto in = fopen((path + "mlboot_data.tsv").c_str(), "r");
	auto out = fopen((path + "data_wo345.tsv").c_str(), "w");
	auto out1 = fopen((path + "data_j1.tsv").c_str(), "w");
	auto out2 = fopen((path + "data_j2.tsv").c_str(), "w");
	auto out3 = fopen((path + "data_j3.tsv").c_str(), "w");
	
	char cuid[40];
	int cat_feature;
	char j1[50000], j2[50000], j3[50000];
	int dt_diff;

	for (int i = 1; fscanf(in, "%s %d %s %s %s %d", cuid, &cat_feature, j1, j2, j3, &dt_diff) != EOF; i++)
	{
		fprintf(out, "%s\t%d\t%d\n", cuid, cat_feature, dt_diff);
		processJson(j1, out1, i);
		processJson(j2, out2, i);
		processJson(j3, out3, i);
		if (i % 1000000 == 1)
			printf("%d rows complete\n", i);
	}
}
