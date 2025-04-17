# Fortran 学习笔记 - 第四部分：科学计算实践

本部分关注 Fortran 在数值计算中的应用，包括内建函数、数值算法示例、自定义数据类型、动态内存管理及编程规范。

## 4.1 数学内建函数 (Mathematical Intrinsics)

* **概念**: Fortran 内置的函数和子程序，无需 `USE` 即可直接调用，简化数学运算。
* **常用函数**:
    * **三角 (弧度)**: `SIN(X)`, `COS(X)`, `TAN(X)`, `ASIN(X)`, `ACOS(X)`, `ATAN(X)`, `ATAN2(Y, X)`
    * **指数/对数**: `EXP(X)` ($e^X$), `LOG(X)` (自然对数), `LOG10(X)` (常用对数)
    * **平方根**: `SQRT(X)` (X >= 0)
    * **绝对值**: `ABS(X)`
    * **求余**: `MOD(A, P)` (结果符号同 A), `MODULO(A, P)` (结果符号同 P)
    * **类型转换**: `REAL(I)`, `INT(X)`, `DBLE(X)`, `CMPLX(X [,Y])`
    * **最大/最小**: `MAX(a, b, ...)` , `MIN(a, b, ...)`
* **数组**: 大多数数学函数是**元素性 (elemental)** 的，可直接作用于数组，返回同样形状的结果数组。 `B = SIN(A)`。
* **精度**: 返回值精度通常取决于输入参数精度。使用 `REAL(KIND=...)` 或 `DOUBLE PRECISION` 及 `1.0D0` 等确保所需精度。
* **示例**:
    ```fortran
    PROGRAM MathIntrinsicsDemo
      IMPLICIT NONE
      REAL, PARAMETER :: PI = 3.14159265
      REAL :: angle_deg = 60.0
      REAL :: angle_rad, sin_val, cos_val
      INTEGER :: i = -7
      REAL :: x = 25.0, sqrt_x

      angle_rad = angle_deg * (PI / 180.0)
      sin_val = SIN(angle_rad)
      cos_val = COS(angle_rad)
      PRINT '(A, F6.2, A, F8.6)', "Angle ", angle_deg, " deg =", angle_rad, " rad"
      PRINT '(A, F8.6, A, F8.6)', "SIN =", sin_val, " COS =", cos_val
      PRINT '(A, F8.6)', "Check: SIN^2 + COS^2 =", sin_val**2 + cos_val**2

      sqrt_x = SQRT(x)
      PRINT '(A, F6.2, A, F8.4)', "SQRT of ", x, " is ", sqrt_x
      PRINT '(A, I3, A, I3)', "ABS of ", i, " is ", ABS(i)
      PRINT '(A, I3)', "MOD(-7, 3) =", MOD(i, 3)
      PRINT '(A, F6.2)', "MAX(1.0, -5.0, 3.0) =", MAX(1.0, -5.0, 3.0)
    END PROGRAM MathIntrinsicsDemo
    ```

## 4.2 简单数值算法示例

* **目的**: 应用 Fortran 知识解决数值问题。
* **示例：数值积分 (梯形法则)**
    * **概念**: 用梯形面积和近似曲线下面积 $\int_a^b f(x) dx$。
    * **公式**: $\int \approx h \times [ \frac{f(a)+f(b)}{2} + \sum_{i=1}^{N-1} f(a+i h) ]$, 其中 $h=(b-a)/N$。
    * **代码**:
        ```fortran
        PROGRAM TrapezoidalIntegration
          IMPLICIT NONE
          REAL :: a=0.0, b=1.0, h, integral_sum, x_i
          INTEGER :: n_intervals=1000, i

          h = (b - a) / REAL(n_intervals)
          integral_sum = (my_function(a) + my_function(b)) / 2.0
          DO i = 1, n_intervals - 1
            x_i = a + REAL(i) * h
            integral_sum = integral_sum + my_function(x_i)
          END DO
          integral_sum = integral_sum * h

          PRINT '(A, F12.8)', "Approximate integral (x^2 from 0 to 1) =", integral_sum
          PRINT '(A, F12.8)', "Exact integral (1/3)                =", 1.0/3.0
        CONTAINS
          REAL FUNCTION my_function(x)
            REAL, INTENT(IN) :: x
            my_function = x**2
          END FUNCTION my_function
        END PROGRAM TrapezoidalIntegration
        ```

## 4.3 派生类型 (Derived Types / Structures)

* **概念**: 用户自定义的数据类型，将不同类型的变量组合成一个逻辑单元。类似 C `struct`。
* **定义**: 使用 `TYPE ... END TYPE` 包含组件声明。
    ```fortran
    TYPE :: Point
      REAL :: x = 0.0, y = 0.0 ! 可以有默认值
    END TYPE Point
    ```
* **声明**: `TYPE(TypeName) :: variable_name`
    ```fortran
    TYPE(Point) :: p1, p2
    ```
* **访问组件**: 使用 `%` 操作符。 `p1 % x = 10.0`。
* **初始化**:
    * 结构构造器: `p1 = Point(1.0, 2.0)` (按顺序提供所有组件值)。
    * 逐个赋值: `p2 % x = 5.0; p2 % y = -1.0`。
* **示例**:
    ```fortran
    PROGRAM DerivedTypeDemo
      TYPE :: Vector2D
         REAL :: x_comp = 0.0
         REAL :: y_comp = 0.0
      END TYPE Vector2D
      IMPLICIT NONE
      TYPE(Vector2D) :: pos = Vector2D(10.0, 5.0), vel

      vel % x_comp = 1.0
      vel % y_comp = -1.0

      PRINT '(A, F6.2, A, F6.2)', "Position: X=", pos%x_comp, ", Y=", pos%y_comp
      PRINT '(A, F6.2, A, F6.2)', "Velocity: X=", vel%x_comp, ", Y=", vel%y_comp
    END PROGRAM DerivedTypeDemo
    ```

## 4.4 动态内存 (可分配数组 - Allocatable Arrays)

* **概念**: 数组大小在运行时确定，而不是编译时。用于处理大小未知的数据。
* **声明**: 使用 `ALLOCATABLE` 属性和冒号 `:` 占位符。
    ```fortran
    REAL, DIMENSION(:), ALLOCATABLE :: data_vec
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: matrix
    ```
* **分配**: `ALLOCATE(array_name(size1, ...), STAT=status_var)`。`STAT` 用于错误检查 (0=成功)。
    ```fortran
    ALLOCATE(data_vec(100), STAT=istat)
    IF (istat /= 0) STOP "Allocation failed!"
    ```
* **释放**: `DEALLOCATE(array_name, STAT=status_var)`。释放不再需要的内存。
* **检查状态**: `ALLOCATED(array_name)` 返回 `.TRUE.` 或 `.FALSE.`。
* **使用**: 分配后如同静态数组使用。
* **示例**:
    ```fortran
    PROGRAM AllocatableDemo
      IMPLICIT NONE
      REAL, DIMENSION(:), ALLOCATABLE :: values
      INTEGER :: n, i, stat
      PRINT *, "Enter number of values:" ; READ *, n
      IF (n <= 0) STOP "Size must be positive"

      ALLOCATE(values(n), STAT=stat)
      IF (stat /= 0) STOP "Allocation failed"
      PRINT *, "Allocated array of size", SIZE(values)

      DO i = 1, n; values(i) = REAL(i); END DO
      PRINT *, "Sum =", SUM(values)

      DEALLOCATE(values, STAT=stat)
      IF (stat /= 0) PRINT *, "Deallocation failed?"
      IF (.NOT. ALLOCATED(values)) PRINT *, "Array deallocated."
    END PROGRAM AllocatableDemo
    ```

## 4.5 库简介 (BLAS/LAPACK)

* **概念**: 使用预编译的、高度优化的外部函数/子程序库来执行常见任务（尤其线性代数）。
* **优点**: 性能高、可靠、标准化、提高开发效率。
* **BLAS**: 基础线性代数子程序 (向量-向量, 矩阵-向量, 矩阵-矩阵)。
* **LAPACK**: 高级线性代数包 (解线性方程组, 特征值, SVD 等)，基于 BLAS。
* **使用**: 查找文档 -> Fortran 中 `CALL` 例程 -> 编译时**链接**库 (例如 `gfortran ... -llapack -lblas` 或使用 MKL 等特定库的链接选项)。链接细节复杂且依赖系统。
* **要点**: 知道它们的存在，在进行密集线性代数计算时优先考虑使用。

## 4.6 编程规范与调试策略

* **规范目的**: 提高代码可读性、可维护性、可靠性。
* **良好实践**:
    * **可读性**: 有意义命名、一致缩进、合理使用空格/空行、模块化。
    * **注释**: 解释“为什么”而非“是什么”，保持更新。
    * **强制**: `IMPLICIT NONE`, 参数 `INTENT`。
    * **现代**: 使用模块，避免 `GOTO`, `COMMON` 等过时特性。
* **调试**: 查找和修复错误。
    * **编译时错误**: 仔细阅读编译器消息，使用 `-Wall` 等警告选项。
    * **运行时错误**:
        * 编译时检查: `gfortran -fcheck=all` 等。
        * `PRINT` 调试: 插入 `PRINT` 语句追踪变量值和流程。
            ```fortran
            PRINT *, "DEBUG: Value before loop =", x
            ! ... loop ...
            PRINT *, "DEBUG: Value after loop =", x
            ```
        * **调试器 (gdb)**: 编译加 `-g`，使用 `break`, `run`, `print`, `next`, `step`, `continue` 等命令检查程序状态。功能强大但有学习曲线。
    * **逻辑错误**: 程序运行但结果错误。需要分析算法、检查中间值、使用简单测试用例。
* **核心**: 好代码易调试。

---
