Matrix and Diagonalization

1. 输入原始数据集$X \in R^{n \times d}$, 其中n是样本数，d是特征维度
为了避免特征尺度过大，需要进行标准化处理：
$$
x^\prime_{ij} = \frac{x_{ij}-\mu_j}{\sigma_j}
$$

2. 计算协方差矩阵

$$
C = \frac{1}{n-1}(X^\prime)^TX^\prime
$$

3. 奇异值分解（SVD）
$$
X^\prime = U\sum V^T
$$

4. 选择主成分
- 将特征值从大到小排序，选择前k哥最大特征值对应的特征向量

$$
V_k = [v_1, v_2,...,v_k] \\
累计贡献率 = \frac{\sum_{i=1}^k\lambda_i}{\sum_{j=1}^d\lambda_j}
$$

5. 数据降维

$$
Z = X^\prime V_k
$$


## 奇异值分解（SVD）

$$
X = U\sum V^T
$$

$U \in R^{m \times m}$ : 左奇异向量（正交矩阵）

$\sum \in R^{m \times n}$：奇异值对角矩阵（非负实数，按降序排列）

$V \in R^{n \times n}$：右奇异向量（正交矩阵）

1. 有$X \in R^{m \times n}$

2. 计算$X^TX$和$XX^T$
    - $X^TX \in R^{n \times n}$
    - $XX^T \in R^{m \times m}$
    - 它们都是对称半正定矩阵

3. 对$X^TX$做特征分解
    - 得到特征值$\lambda_i$和特征向量$v_i$
    - 特征向量$v_i$构成矩阵V

4. 计算奇异值
    - 每个奇异值$\sigma_i = \sqrt{\lambda_i}$（非负）
    - 把这些值放到对角矩阵$\sum$里

5. 求左奇异向量$U$
    - 用公式：$u_i = \frac{1}{\sigma_i}Xv_i$

6. 得到分解结果
    - $X = U\sum V^T$

## 求特征值和特征向量

1. 定义公式

    $A \in R^{n \times n}$

    如果存在非零向量$v$和标量$\lambda$，使得
    $Av = \lambda v$
    - $\lambda$就叫特征值
    - $v$就叫特征向量

2. 计算步骤

    - 写出特征方程
    $det(A - \lambda I) = 0$

    - 解方程得到所有的特征值$\lambda_1, \lambda_2, ..., \lambda_n$
    - 代入求向量，对每个特征值$\lambda_i$，解方程$(A - \lambda_i I)v_i = 0$

$$

$$

$\lambda$就是特征值



$$
A = PDP^T, P^TP = I, P^T = p^{-1} \\
A : (n*n) P:(n*n) D: diagonal

$$





$$
<x - \lambda b, b> = 0 \\

(x_1 - \lambda b_1)b_1 + (x_2 - \lambda b_2)b_2 + ... + (x_n - \lambda b_n)b_n = 0 \\

x_1b_1-\lambda b_1^2 + x_2b_2-\lambda b_2^2 + ... + x_nb_n-\lambda b_n^2 = 0 \\

x_1b_1 + ... + x_nb_n - \lambda(b_1^2 + ... + b_n^2) = 0 \\

\lambda = \frac{x_1b_1 + ... + x_nb_n}{b_1^2 + ... + b_n^2} \\

\lambda = \frac{<b, x>}{||b||^2} = \frac{b^Tx}{||b||^2}
$$