# Fortran 学习笔记 - 第三部分：模块化编程与数据处理

本部分重点学习如何将 Fortran 代码组织成更易于管理和重用的模块，以及如何进行基本的文件操作。

## 3.1 程序单元 - 函数 (Function) 与 子程序 (Subroutine)

* **模块化编程**: 将大程序分解为小的、功能独立的**程序单元 (Procedures)** 或**子程序 (Subprograms)**，以提高代码的可读性、可维护性和可重用性。
* **两种主要类型**: 函数 (`FUNCTION`) 和 子程序 (`SUBROUTINE`)。
* **函数 (`FUNCTION`)**:
    * **目的**: 计算并返回**一个单一值** (通过函数名本身)。
    * **结构**:
        ```fortran
        [类型] FUNCTION 函数名(参数...)
          IMPLICIT NONE
          ! 声明 (含 INTENT)
          ! ... 计算 ...
          函数名 = 结果值 ! 结果赋给函数名
        END FUNCTION 函数名
        ```
    * **调用**: 在表达式中使用，如 `y = MyFunc(x)`。
    * **示例: 计算圆面积**
        ```fortran
        PROGRAM FunctionDemo
          IMPLICIT NONE
          REAL :: radius = 2.5, area
          area = CircleArea(radius) ! 调用函数
          PRINT *, "Radius =", radius, ", Area =", area
        CONTAINS ! ---- Internal function starts here ----
          REAL FUNCTION CircleArea(r)
            IMPLICIT NONE
            REAL, INTENT(IN) :: r  ! r 是输入参数
            REAL, PARAMETER :: PI = 3.14159265
            IF (r < 0.0) THEN
                CircleArea = 0.0 ! 半径不能为负，返回 0
            ELSE
                CircleArea = PI * r**2 ! 计算结果赋给函数名
            END IF
          END FUNCTION CircleArea
        END PROGRAM FunctionDemo
        ```
* **子程序 (`SUBROUTINE`)**:
    * **目的**: 执行一系列操作，可能修改参数或执行 I/O，**不通过名字返回值**。
    * **结构**:
        ```fortran
        SUBROUTINE 子程序名(参数...)
          IMPLICIT NONE
          ! 声明 (含 INTENT)
          ! ... 操作 ...
          ! 可能修改 INTENT(OUT) 或 INTENT(INOUT) 参数
        END SUBROUTINE 子程序名
        ```
    * **调用**: 使用 `CALL` 语句，如 `CALL MySub(a, b)`。
    * **示例: 交换两个实数的值**
        ```fortran
        PROGRAM SubroutineDemo
          IMPLICIT NONE
          REAL :: a = 5.0, b = 10.0
          PRINT *, "Before CALL: a =", a, ", b =", b
          CALL Swap(a, b) ! 调用子程序
          PRINT *, "After CALL:  a =", a, ", b =", b
        CONTAINS ! ---- Internal subroutine starts here ----
          SUBROUTINE Swap(x, y)
            IMPLICIT NONE
            REAL, INTENT(INOUT) :: x, y ! x, y 是输入输出参数，它们的值会被改变
            REAL :: temp              ! 局部临时变量
            temp = x
            x = y
            y = temp
          END SUBROUTINE Swap
        END PROGRAM SubroutineDemo
        ```
* **核心区别**: 函数像数学函数返回值，用在表达式里；子程序执行动作，用 `CALL` 调用。

## 3.2 参数 (Arguments) 与 INTENT 属性

* **参数**: 程序单元与调用者交换信息的方式。
    * **虚拟参数 (Dummy Arguments)**: 定义程序单元时使用的占位符名称。
    * **实际参数 (Actual Arguments)**: 调用程序单元时传入的实际变量或值。类型、种类、秩需匹配。
* **`INTENT` 属性**: 声明虚拟参数的**用途 (意图)**。
    * **`INTENT(IN)`**: 参数仅用于**输入**，在单元内部**不可修改**。
    * **`INTENT(OUT)`**: 参数仅用于**输出**，单元内部**必须赋值**，传入值无意义。
    * **`INTENT(INOUT)`**: 参数**既是输入也是输出**，可读可写。
* **示例: 计算和与积**
    ```fortran
    PROGRAM IntentOutDemo
      IMPLICIT NONE
      REAL :: val1 = 5.0, val2 = 3.0
      REAL :: sum_res, prod_res ! Variables to receive output
      CALL CalculateSumProd(val1, val2, sum_res, prod_res)
      PRINT *, "Val1=", val1, "Val2=", val2
      PRINT *, "Sum =", sum_res, "Product =", prod_res
    CONTAINS
      SUBROUTINE CalculateSumProd(num1, num2, sum_out, prod_out)
        IMPLICIT NONE
        REAL, INTENT(IN) :: num1, num2 ! Inputs
        REAL, INTENT(OUT) :: sum_out, prod_out ! Outputs
        sum_out = num1 + num2  ! Must assign values to OUT arguments
        prod_out = num1 * num2 ! Must assign values to OUT arguments
      END SUBROUTINE CalculateSumProd
    END PROGRAM IntentOutDemo
    ```
* **建议**: 总是为虚拟参数明确指定 `INTENT`。

## 3.3 模块 (Module) - 组织程序单元和数据

* **目的**: 将相关的过程、数据、类型定义等**封装**成一个可重用的单元。是现代 Fortran 代码组织的**核心**。
* **优点**: 提高重用性、可维护性，控制访问权限，避免命名冲突，取代 `COMMON` 等旧方法。
* **结构**:
    ```fortran
    MODULE MyMathLibrary
      IMPLICIT NONE
      PRIVATE ! [可选] 默认私有
      PUBLIC :: PI, CalculateCircumference, PrintLastResult ! 公开接口

      REAL, PARAMETER :: PI = 3.1415926535
      REAL, SAVE :: last_result = 0.0 ! Module variable

    CONTAINS
      FUNCTION CalculateCircumference(radius) RESULT(circumference)
        REAL :: circumference
        REAL, INTENT(IN) :: radius
        circumference = 2.0 * PI * radius
        last_result = circumference
      END FUNCTION CalculateCircumference

      SUBROUTINE PrintLastResult()
        PRINT *, "Last calculated result was:", last_result
      END SUBROUTINE PrintLastResult
    END MODULE MyMathLibrary
    ```
* **关键**: `CONTAINS` 分隔声明和实现；`PUBLIC`/`PRIVATE` 控制可见性；模块变量（`CONTAINS` 前声明）可被模块内过程共享（通常隐式 `SAVE`）。
* **使用**: 在需要的地方用 `USE MyModule` 或 `USE MyModule, ONLY: Item1, Item2` 导入。
    ```fortran
    PROGRAM UseModuleDemo
      USE MyMathLibrary, ONLY: PI, CalculateCircumference, PrintLastResult
      IMPLICIT NONE
      REAL :: r = 10.0, c
      c = CalculateCircumference(r)
      PRINT *, "Circumference:", c
      PRINT *, "PI:", PI
      CALL PrintLastResult()
    END PROGRAM UseModuleDemo
    ```
* **编译**: 通常需要先编译模块文件 (`.f90`) 生成 `.mod` 文件，再编译使用该模块的文件。

## 3.4 作用域 (Scope) 与 SAVE 属性

* **作用域**: 名字（变量、过程等）有效的代码范围。
    * **局部作用域**: 在过程内部声明的变量，仅在该过程内有效，调用结束通常失效。
    * **模块作用域**: 在模块内声明的实体，在模块内有效，`PUBLIC` 的可通过 `USE` 在外部访问。
* **`SAVE` 属性**:
    * **目的**: 使过程内的**局部变量**在多次调用之间**保持其值**，而不是每次调用都重置。
    * **语法**: `类型, SAVE :: 变量名 [= 初始值]` 或 `SAVE :: 变量名`。
    * **用途**: 计数器、状态保持、一次性初始化等。
    * **模块变量**: 通常行为类似自带 `SAVE`。
* **示例**:
    ```fortran
    PROGRAM ScopeSaveDemo
      IMPLICIT NONE; INTEGER :: i
      PRINT *, "Calling counter:"
      DO i = 1, 3; CALL Counter; END DO
    CONTAINS
      SUBROUTINE Counter
        IMPLICIT NONE
        INTEGER, SAVE :: n = 0 ! n 的值会跨调用保留并累加
        n = n + 1
        PRINT *, "Counter value:", n
      END SUBROUTINE Counter
    END PROGRAM ScopeSaveDemo ! 输出会是 1, 2, 3
    ```

## 3.5 文件 I/O 基础 (OPEN, CLOSE, READ, WRITE)

* **概念**: 程序与外部文件进行数据交换。
* **单元号**: 一个整数，程序通过它引用一个打开的文件（避免使用 5, 6, `*`）。
* **`OPEN`**: 连接文件和单元号。
    * `OPEN(UNIT=u, FILE=fname, STATUS=stat, ACTION=act, IOSTAT=io)`
    * `STATUS`: `'OLD'`, `'NEW'`, `'REPLACE'`, `'SCRATCH'`, `'UNKNOWN'`。
    * `ACTION`: `'READ'`, `'WRITE'`, `'READWRITE'`。
    * `IOSTAT`: 整数变量，0 表示成功，>0 表示错误。**必用**。
* **`CLOSE`**: 断开连接。
    * `CLOSE(UNIT=u, STATUS=keep_or_delete, IOSTAT=io)`
    * `STATUS`: `'KEEP'` (默认), `'DELETE'`。
* **`WRITE` (文件)**: 向文件写入。
    * `WRITE(UNIT=u, FMT=fmt, IOSTAT=io) ...`
    * `FMT` 可以是格式字符串/标签，或 `*` (列表导向)。
* **`READ` (文件)**: 从文件读取。
    * `READ(UNIT=u, FMT=fmt, IOSTAT=io) ...`
    * `IOSTAT`: <0 通常表示文件结束 (EOF)，>0 表示错误。需要检查。
* **示例: 写入并读回文件**
    ```fortran
    PROGRAM BasicFileIO
      IMPLICIT NONE
      INTEGER, PARAMETER :: MY_UNIT = 15       ! 定义一个单元号
      CHARACTER(LEN=50) :: fname = "data.tmp" ! 文件名
      INTEGER :: i, id_write, id_read, io_stat
      REAL :: val_write, val_read

      ! --- 1. 打开文件用于写入 (如果存在则替换) ---
      OPEN(UNIT=MY_UNIT, FILE=fname, STATUS='REPLACE', ACTION='WRITE', IOSTAT=io_stat)
      IF (io_stat /= 0) THEN
        PRINT *, "错误：无法打开文件 ", TRIM(fname), " 用于写入. IOSTAT=", io_stat
        STOP 1 ! 停止程序，返回错误码 1
      END IF
      PRINT *, "文件 ", TRIM(fname), " 已打开用于写入."

      ! --- 2. 向文件写入数据 ---
      DO i = 1, 5
        id_write = i
        val_write = REAL(i) * 10.0
        WRITE(MY_UNIT, FMT='(I5, 2X, F8.2)', IOSTAT=io_stat) id_write, val_write ! 格式化写入
        IF (io_stat /= 0) THEN
          PRINT *, "错误：写入文件时出错. IOSTAT=", io_stat
          CLOSE(MY_UNIT) ! 尝试关闭文件
          STOP 2
        END IF
      END DO
      PRINT *, "数据已写入文件."

      ! --- 3. 关闭文件 ---
      CLOSE(UNIT=MY_UNIT, IOSTAT=io_stat)
      IF (io_stat /= 0) THEN
        PRINT *, "错误：关闭文件时出错. IOSTAT=", io_stat
        STOP 3
      END IF
      PRINT *, "文件已关闭."

      ! --- 4. 重新打开同一个文件用于读取 ---
      OPEN(UNIT=MY_UNIT, FILE=fname, STATUS='OLD', ACTION='READ', IOSTAT=io_stat)
      IF (io_stat /= 0) THEN
        PRINT *, "错误：无法打开文件 ", TRIM(fname), " 用于读取. IOSTAT=", io_stat
        STOP 4
      END IF
      PRINT *, "文件 ", TRIM(fname), " 已重新打开用于读取."

      ! --- 5. 从文件读取数据直到文件末尾 ---
      PRINT *, "从文件读取的数据:"
      DO
        READ(MY_UNIT, FMT='(I5, 2X, F8.2)', IOSTAT=io_stat) id_read, val_read ! 格式化读取
        IF (io_stat < 0) THEN  ! 小于 0 通常表示 EOF
          PRINT *, "...已到达文件末尾."
          EXIT ! 正常退出读取循环
        ELSE IF (io_stat > 0) THEN ! 大于 0 表示读取错误
          PRINT *, "错误：读取文件时出错. IOSTAT=", io_stat
          EXIT ! 错误退出读取循环
        END IF
        ! 如果 io_stat == 0, 表示成功读取一行
        PRINT *, "ID:", id_read, " Value:", val_read
      END DO

      ! --- 6. 关闭文件 ---
      CLOSE(UNIT=MY_UNIT, STATUS='KEEP', IOSTAT=io_stat) ! 保留文件
      IF (io_stat /= 0) THEN
        PRINT *, "错误：读取后关闭文件时出错. IOSTAT=", io_stat
        STOP 5
      END IF
      PRINT *, "文件已关闭 (保留)."
      PRINT *, "文件 I/O 演示完成."

    END PROGRAM BasicFileIO
    ```

---
