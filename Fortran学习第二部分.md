
# Fortran 学习笔记 - 第二部分：核心编程结构

本部分涵盖了 Fortran 中控制程序流程和组织数据的核心结构，包括选择、循环、数组及格式化输入输出。

## 2.1 控制流程 - 选择结构 (IF 语句)

* **作用**: 让程序根据条件 (`.TRUE.` 或 `.FALSE.`) 选择执行不同的代码路径。
* **形式**:
    * **`IF (条件) THEN ... END IF`**: 条件为真时执行块内代码。
        ```fortran
        IF (temperature > 30.0) THEN
          PRINT *, "Warning: Temperature is high!"
        END IF
        ```
    * **`IF (条件) THEN ... ELSE ... END IF`**: 条件为真执行 `THEN` 块，为假执行 `ELSE` 块。
        ```fortran
        IF (score >= 60) THEN
          PRINT *, "Passed."
        ELSE
          PRINT *, "Failed."
        END IF
        ```
    * **`IF (条件1) THEN ... ELSE IF (条件2) THEN ... ELSE ... END IF`**: 依次检查条件，执行第一个为真的分支，若都为假则执行可选的 `ELSE`。
        ```fortran
        IF (grade >= 90) THEN
          PRINT *, "A"
        ELSE IF (grade >= 80) THEN
          PRINT *, "B"
        ELSE
          PRINT *, "C or lower"
        END IF
        ```
    * **嵌套 `IF`**: `IF` 结构内可以包含其他 `IF` 结构。
        ```fortran
        PROGRAM NestedIfDemo
          IMPLICIT NONE
          INTEGER :: age = 67, dayOfWeek = 2
          REAL :: price = 100.0
          IF (age >= 65) THEN ! Outer IF
            price = price * 0.90
            IF (dayOfWeek == 2) THEN ! Inner IF
              price = price * 0.95
            END IF
          END IF
          PRINT *, "Final Price:", price
        END PROGRAM NestedIfDemo
        ```

## 2.2 控制流程 - 循环结构 (DO 循环)

* **作用**: 重复执行一段代码。
* **计数 `DO` 循环**:
    * 语法: `DO counter = start, end, [step] ... END DO`
    * `counter` 变量从 `start` 变化到 `end`（含），每次增量为 `step`（默认为 1）。
    * 示例 (1 到 5):
        ```fortran
        INTEGER :: i
        DO i = 1, 5
          PRINT *, i
        END DO
        ```
    * 示例 (10 到 0, 步长 -2):
        ```fortran
        INTEGER :: j
        DO j = 10, 0, -2
          PRINT *, j
        END DO
        ```
* **循环控制**:
    * `EXIT`: 立即跳出整个 `DO` 循环。
        ```fortran
        DO k = 1, 10
          IF (k > 3) EXIT
          PRINT *, k ! Prints 1, 2, 3
        END DO
        ```
    * `CYCLE`: 跳过当前迭代的剩余部分，开始下一次迭代。
        ```fortran
        DO m = 1, 5
          IF (MOD(m, 2) == 0) CYCLE ! Skip even numbers
          PRINT *, m ! Prints 1, 3, 5
        END DO
        ```
* **其他**: 还有 `DO WHILE` 循环（条件为真时执行）。

## 2.3 一维数组 (1D Arrays)

* **概念**: 存储**同类型**元素的有序集合，通过**索引**访问。
* **声明**:
    * `类型, DIMENSION(大小) :: 数组名` 或 `类型 :: 数组名(大小)` (索引 1 到 大小)
    * `类型 :: 数组名(下界:上界)`
        ```fortran
        REAL, DIMENSION(10) :: x
        INTEGER :: counts(0:4) ! 索引 0, 1, 2, 3, 4
        ```
* **访问**: `数组名(索引)`，索引必须在边界内。
    ```fortran
    x(1) = 10.5
    PRINT *, counts(0)
    ```
* **初始化**:
    * 构造器: `(/ 值1, 值2, ... /)` 或 `[ 值1, 值2, ... ]` (F2003+)
        ```fortran
        REAL, DIMENSION(3) :: vec = (/ 1., 2., 3. /)
        ```
    * `DO` 循环: 逐个赋值。
    * 隐式 `DO`: `(/ (表达式, index = start, end) /)`
        ```fortran
        INTEGER :: squares(5) = (/ (i*i, i=1,5) /) ! 1, 4, 9, 16, 25
        ```
* **示例**:
    ```fortran
    PROGRAM ArrayDemo1D
      IMPLICIT NONE
      INTEGER, PARAMETER :: AS = 5
      REAL, DIMENSION(AS) :: data = (/ 1., 2., 3., 4., 5. /)
      INTEGER :: i
      REAL :: total = 0.0
      DO i = 1, AS
        total = total + data(i)
      END DO
      PRINT *, "Total:", total, " Average:", total / REAL(AS)
    END PROGRAM ArrayDemo1D
    ```

## 2.4 多维数组 (Multi-dimensional Arrays)

* **概念**: 使用多个索引访问元素，表示矩阵、网格等。
* **声明**: `类型, DIMENSION(大小1, 大小2, ...)` 或 `类型 :: 数组名(下1:上1, 下2:上2, ...)`
    ```fortran
    REAL, DIMENSION(3, 4) :: matrix ! 3x4 矩阵
    ```
* **存储顺序**: **列优先 (Column-Major)**！第一个索引变化最快。`matrix(1,1), matrix(2,1), matrix(3,1), matrix(1,2), ...`
* **访问**: `数组名(索引1, 索引2, ...)`
    ```fortran
    matrix(2, 3) = 10.0
    ```
* **初始化**:
    * `RESHAPE`: 将一维构造器重塑为多维（按列优先填充）。
        ```fortran
        INTEGER, DIMENSION(2, 3) :: t
        t = RESHAPE( (/ 1, 2, 3, 4, 5, 6 /), (/ 2, 3 /) ) ! [[1, 3, 5], [2, 4, 6]]
        ```
    * 嵌套 `DO` 循环（推荐内层循环对应第一个索引）。
        ```fortran
        INTEGER :: i, j
        DO j = 1, 4 ! 列在外
          DO i = 1, 3 ! 行在内
            matrix(i, j) = REAL(i + j)
          END DO
        END DO
        ```
* **示例**:
    ```fortran
    PROGRAM ArrayDemo2D
      IMPLICIT NONE
      INTEGER, PARAMETER :: R=2, C=3
      REAL, DIMENSION(R, C) :: mat
      INTEGER :: i, j
      DO j = 1, C; DO i = 1, R; mat(i, j) = i*10 + j; END DO; END DO
      PRINT *, "Matrix:"
      DO i = 1, R; PRINT '(3F6.1)', (mat(i, j), j=1, C); END DO
    END PROGRAM ArrayDemo2D
    ```

## 2.5 数组操作 (Array Operations)

* **概念**: 对整个数组或数组节进行操作，无需显式循环。
* **元素级运算**:
    * 数组-数组 (形状需相同): `C = A + B`
    * 标量-数组: `Y = X * 2.0`
* **数组节/切片**: `数组名(下:上:步, ...)`
    * `:` 代表整个维度范围。
    * `V(2:5)`, `M(:, 1)` (第一列), `M(2, :)` (第二行), `M(1:2, 1:2)` (子矩阵)
    * 可用于赋值左侧 `M(:, 1) = 0.0`。
* **常用内建函数**:
    * 查询: `SIZE`, `SHAPE`
    * 规约: `SUM`, `PRODUCT`, `MAXVAL`, `MINVAL`, `COUNT` (可沿指定维度 `dim=`)
    * 构造/操作: `RESHAPE`, `TRANSPOSE`
    * 位置: `MAXLOC`, `MINLOC`
    * 数学: `MATMUL` (矩阵乘), `DOT_PRODUCT` (点积)
* **示例**:
    ```fortran
    PROGRAM ArrayOperationsDemo
      IMPLICIT NONE
      REAL, DIMENSION(4) :: a = (/ 1., 2., 3., 4. /), b
      b = a * 10.0       ! b = (/ 10., 20., 30., 40. /)
      PRINT *, SUM(a)    ! 10.0
      PRINT *, COUNT(a > 2.5) ! 2
      PRINT *, MAXVAL(a) ! 4.0
      PRINT *, MAXLOC(a) ! [4] (索引位置)
    END PROGRAM ArrayOperationsDemo
    ```

## 2.6 格式化输入/输出 (FORMAT 与格式描述符)

* **概念**: 精确控制数据输入输出的布局、精度和样式。区别于列表导向 (`*`)。
* **指定格式**:
    * `FORMAT` 语句: `PRINT label, ...` / `label FORMAT(...)`
    * 字符串常量: `PRINT '(...)' , ...`
    * `WRITE(*, format) ...` / `READ(*, format) ...`
* **常用描述符**:
    * `I<w>`: 整数，宽 `w`
    * `F<w>.<d>`: 实数，定点，宽 `w`，小数 `d` 位
    * `E<w>.<d>` 或 `ES<w>.<d>`: 实数，科学计数法，宽 `w`，尾数小数 `d` 位
    * `A<w>` 或 `A`: 字符串，宽 `w` 或自动长度
    * `L<w>`: 逻辑 (T/F)，宽 `w`
    * `nX`: n 个空格
    * `/`: 换行
    * `T<c>`: 定位到 c 列
    * `'text'`: 输出字面文本
    * 重复: `n<desc>` 或 `n(...)`
* **示例**:
    ```fortran
    PROGRAM FormattedOutputDemo
      IMPLICIT NONE
      INTEGER :: i=123; REAL :: x=12.3456
      PRINT '(A, I5, A, F8.3, A)', " Int:", i, " Real:", x, "."
      ! 输出: Int:  123 Real:  12.346. (F会四舍五入)
    END PROGRAM FormattedOutputDemo
    ```

---
