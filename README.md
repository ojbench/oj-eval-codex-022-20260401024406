# Problem 022 - Guess

**ACMOJ Problem ID**: 1936

## Table of Contents

- [Problem 022 - Guess](#problem-022-guess)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
    - [Background](#background)
  - [Assignment Description](#assignment-description)
    - [Grade Composition](#grade-composition)
  - [Assignment Requirements](#assignment-requirements)
    - [Input Format](#input-format)
    - [Output Format](#output-format)
    - [Samples](#samples)
    - [Data Constraints](#data-constraints)
  - [Per-Testcase Resource Limits](#per-testcase-resource-limits)
  - [Test Data](#test-data)
  - [Submission Requirements](#submission-requirements)
    - [Submission Format](#submission-format)
    - [Submission Guidelines](#submission-guidelines)
    - [Evaluation Notes](#evaluation-notes)
    - [Academic Integrity](#academic-integrity)

## Introduction

### Background

Interactive problem: implement a guessing algorithm. Requires implementing source.h with query interface. Git project.

## Assignment Description

### Grade Composition

| Grading Component | Percentage |
| :--: | :--: |
| Pass **1936. Guess** | 80% |
| Code Review | 20% |

Here are several points that need clarification:

- In the Code Review, we will **strictly examine your code style and repository organization structure, etc.**

- This assignment provides some sample data for testing, stored in the `/workspace/data/022/data_test/` directory. Note that these are not the test cases on the Online Judge. Passing all local test cases does not guarantee that you will pass the OJ tests.

- Besides the provided sample data, we also encourage you to design your own test data based on your program logic to assist debugging.

## Assignment Requirements

### Problem Description

这是一道交互题。

有一个长度为 $n$ 且元素互不相同的数组 A（标号为 $1\to n$）。你想知道每个元素是多少。你可以进行若干次询问，每次选择三个不同的数 $x,y,z\in [1,n]$，你将会得到 $\max\\{A_x,A_y,A_z\\}+\min\\{A_x,A_y,A_z\\}$。在知道 A 数组每个元素是多少的同时，你也想达成询问次数最少的成就。

附件下载：https://github.com/ACMClassCourse-2022/ppca-5a-guess

### 交互方法

+ 你需要在 `source.h` 中实现函数 `int guess(int n, int Taskid)`

  + 其中 $n$ 为数组长度，$Taskid$ 是子任务编号。当你得到 A 数组每个元素的值后，函数的返回值应当为 $\sum_{i=1}^{n}A_i\times 233^i\pmod{998244353}$。

  + **注意本题有多组数据，因此交互库会对这个函数进行多次调用**。


+ 你可以调用接口 `int query(int x, int y, int z)`

  + 其意义见题目描述。注意如果你给出的参数不合法，将出现 Runtime Error.


+ **你只需提交 `source.h` 文件即可。**


### 本地测试
你可以用 `exec.sh` 来对程序进行编译。编译得到的可执行文件的输入输出格式如下：

#### 本地测试输入

第一行三个整数 $T, Taskid, Maxscore$，表示数据组数，子任务编号，和测试点分值。

接下来有 $T$ 组数据，每组数据第一行一个整数 $n$，第二行 $n$ 个整数表示 $A$ 数组。

#### 本地测试输出

输出到标准输出中。

若选手猜错，将输出 `wa x`，其中x为猜错的数据编号。

否则将输出 `ok S`，其中 $S$ 表示选手在该测试点获得得分。

### 注意
+ 请不要在你实现的函数中进行输入输出，否则会导致得 0 分。
+ 请不要使用除了 `query` 外的其他交互库中的变量或函数，最终评测时使用的 `grader.cpp` 将有所不同。
+ 交互库不是 adaptive 的，即 A 序列是初始确定的，不会因为你的询问而发生改变。



### 评分方式

对于某组数据，设 $P=\min\\{n+35,2\times n\\}$，而你使用了 $x$  次询问猜出序列，那么你的得分 $S$ 为：

$$
S=\mathrm{Maxscore}\times\begin{cases}
1,&x\leq P\\\\
0.9-0.6\times\frac{x-P}{P},&P<x\le 1.5\times P\\\\
0.7-0.2\times\frac{x-P}{P},&1.5\times P< x \leq 3\times P\\\\
\max\\{0.1,0.34-0.02\times\frac{x-P}{P}\\},&x>3\times P\\\\
\end{cases}
$$

一个测试点的得分为各组数据得分的最小值。任意一次猜错将导致测试点得 0 分。

### Input Format

See the problem description above.

### Output Format

See the problem description above.

### Samples

No sample data provided for this problem.

### Data Constraints

对于所有数据，有 $1 ≤ T ≤ 10, 1 ≤ Taskid ≤ 5，5 ≤ n ≤ 10^4, 1 ≤ A_i ≤ 10^9$，$A_i$两两不同。数据保证一定有解。

+ $\text{Subtask1}~(10\\%), A_1=1,A_2=2.$

+ $\text{Subtask2}~(20\\%), A_1=1,A_2=10^9.$

+ $\text{Subtask3}~(20\\%), A_1=2\times 10^8,A_2=5\times 10^8,A_3=8\times 10^8.$

+ $\text{Subtask4}~(20\\%), n=5.$

+ $\text{Subtask5}~(30\\%), 没有特殊限制.$

## Per-Testcase Resource Limits

- **Time Limit (per test case)**: 3000 ms
- **Memory Limit (per test case)**: 128 MiB
- **Disk Usage**: No disk usage is permitted.

## Test Data

The test data for this problem is located at `/workspace/data/022/data_test/`.

## Submission Requirements

### Submission Format

Submit a single C++ source file to the Online Judge. The OJ provides its own `main.cpp` which `#include "src.hpp"`. Your submission will be written to `src.hpp` and compiled together with the OJ's driver code.

### Submission Guidelines

- The submitted code must be able to compile successfully through the above compilation process
- The program needs to be able to read data from standard input and write results to standard output
- Please ensure the code runs correctly within the given time and space limits
- **You must use C++ or C language** to implement this assignment

### Evaluation Notes

- The evaluation system will test your program using the provided test data
- The program output must exactly match the expected output (including format)
- Exceeding time or memory limits will be judged as the corresponding error type
- Please pay attention to the overall time performance of your code and the time complexity of each part of your algorithm.

### Academic Integrity

If any violations are found during evaluation or code review (including but not limited to using unconventional methods to pass test cases), your final score may be significantly reduced or become **0 points**.
