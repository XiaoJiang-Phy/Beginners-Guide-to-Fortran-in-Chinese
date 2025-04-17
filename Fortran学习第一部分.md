
# Fortran 学习笔记 - 第一部分：基础与环境搭建

本笔记总结了 Fortran 学习计划的第一阶段内容，涵盖了 Fortran 的基本概念、环境设置以及核心语法元素。

## 1.1 Fortran 的历史与定位

* **名称来源**: **For**mula **Tran**slation (公式翻译)。
* **诞生**: 20 世纪 50 年代由 IBM 团队（John Backus 领导）开发，旨在简化科学与工程计算编程。
* **里程碑**: 世界上第一个被广泛使用的高级编程语言，极大地提高了科学计算的编程效率。
* **持续进化**: 经历了 FORTRAN 77, Fortran 90/95 (重大革新，引入自由格式、模块、数组运算等), Fortran 2003/2008/2018/2023 (面向对象、C 互操作、并行 Coarray 等) 标准。
* **当今地位**:
    * **高性能**: 编译器优化能力强，数值计算速度快。
    * **遗留代码**: 大量关键科学工程代码库仍在使用和维护。
    * **HPC**: 高性能计算领域的主力语言之一。
    * **数值计算直观**: 语法，特别是现代 Fortran 的数组特性，非常适合数学表达。

## 1.2 程序基本结构

* **核心框架**: 程序通常包含在 `PROGRAM program_name` 和 `END PROGRAM program_name` 语句之间。
* **语句**: 代码由语句组成，通常一行一个语句。Fortran 对关键字和标识符大小写不敏感，但建议保持风格一致。
* **源文件格式**:
    * **固定格式 (Fixed Format)**: 传统格式，对代码列位置有严格要求，现已少用。
    * **自由格式 (Free Format)**: Fortran 90 引入，现代标准。代码可在行内任意位置开始，行长通常可达 132 字符。
* **注释**: 使用 `!` 开始注释，从 `!` 到行尾被编译器忽略。
* **续行**: 在需要续行的代码行末尾加 `&`。

**示例结构:**

```fortran
PROGRAM ProgramName
  ! 这是注释
  IMPLICIT NONE ! 强制显式声明所有变量 (推荐)

  ! 变量声明区
  ! ...

  ! 程序执行代码区
  PRINT *, "Some output" ! 行内注释

END PROGRAM ProgramName
```

## 1.3 安装编译器与设置环境

* **编译器**: 将 Fortran 源代码翻译成机器可执行代码的工具。
* **推荐**: **GFortran** (GNU Fortran)，是 GCC 的一部分，免费、开源、跨平台。
* **安装**:
    * **Linux**: 使用包管理器，如 `sudo apt install gfortran` (Debian/Ubuntu) 或 `sudo dnf install gcc-gfortran` (Fedora)。
    * **macOS**: 使用 Homebrew `brew install gcc` (通常包含 GFortran)。
    * **Windows**: 推荐使用 **MSYS2** (`pacman -S mingw-w64-x86_64-gcc-fortran`) 或 **WSL** (安装 Linux 子系统后按 Linux 方法安装)。安装后可能需要配置环境变量 PATH。
* **验证**: 在终端或命令行运行 `gfortran --version`，看到版本信息即表示成功。
* **编辑器**: 使用任何纯文本编辑器编写代码，如 VS Code (推荐，有 Fortran 插件), Notepad++, Gedit, Vim 等。

## 1.4 编写、编译、运行第一个程序

1.  **编写**: 使用文本编辑器创建文件 (如 `hello.f90`) 并输入以下代码：
    ```fortran
    PROGRAM HelloWorld
      IMPLICIT NONE
      PRINT *, "Hello, World!"
    END PROGRAM HelloWorld
    ```
2.  **编译**: 打开终端，`cd` 到文件所在目录，运行：
    ```bash
    gfortran hello.f90 -o hello
    ```
    (`-o hello` 指定输出的可执行文件名为 `hello`)
3.  **运行**:
    * Linux/macOS: `./hello`
    * Windows: `hello.exe` (或 `hello`)

    终端应输出 `Hello, World!`。

## 1.5 内建数据类型

* **作用**: 告诉编译器变量存储什么种类的数据。
* **`IMPLICIT NONE`**: 强烈推荐使用，强制声明所有变量类型。
* **声明语法**: `类型名 :: 变量名1, 变量名2`
* **主要类型**:
    * `INTEGER`: 整数。
    * `REAL`: 单精度浮点数 (带小数点的数)。
    * `DOUBLE PRECISION`: 双精度浮点数 (精度更高)。
    * `COMPLEX`: 复数 (一对实数)。
    * `LOGICAL`: 逻辑值 (`.TRUE.` 或 `.FALSE.`)。
    * `CHARACTER(LEN=...)`: 固定长度的字符串。 `LEN=` 指定长度。

**声明示例:**

```fortran
PROGRAM DataTypesExample
  IMPLICIT NONE
  INTEGER :: count
  REAL :: temperature
  DOUBLE PRECISION :: exact_value
  COMPLEX :: wave_func
  LOGICAL :: is_finished
  CHARACTER(LEN=50) :: message
END PROGRAM DataTypesExample
```

## 1.6 变量与常量

* **变量 (Variables)**:
    * 带名字的存储位置，其值可以在程序运行时改变。
    * 必须先声明后使用。
    * **命名规则**: 字母开头，可包含字母、数字、下划线，不区分大小写，不能是关键字。
    * **赋值**: 使用 `=` 赋值，如 `variable = expression`。
    * **初始化**: 可在声明时赋值，如 `INTEGER :: count = 0`。
* **常量 (Constants)**:
    * 值在程序运行期间不可改变。
    * **命名常量**: 使用 `PARAMETER` 属性定义，必须在声明时初始化。
    * **优点**: 提高代码可读性和可维护性。
    * **声明语法**: `类型名, PARAMETER :: 常量名 = 值`

**示例:**

```fortran
PROGRAM VariablesAndConstants
  IMPLICIT NONE
  REAL, PARAMETER :: PI = 3.14159
  INTEGER, PARAMETER :: MAX_ITER = 1000
  CHARACTER(LEN=*), PARAMETER :: VERSION = "1.0"

  INTEGER :: current_iteration
  REAL :: radius = 5.0, area

  current_iteration = 0
  area = PI * radius**2

  PRINT *, "Version: ", VERSION
  PRINT *, "Area of circle with radius", radius, " is ", area
  PRINT *, "Maximum iterations:", MAX_ITER

  ! PI = 3.14 ! <--- 非法操作，常量不能被修改
END PROGRAM VariablesAndConstants
```

## 1.7 基本运算符

* **算术运算符**:
    * `+`, `-`, `*`, `/`, `**` (加, 减, 乘, 除, 乘方)。
    * 注意 `/`：整数除法 vs 实数除法。
* **关系运算符**:
    * `==`, `/=`, `<`, `<=`, `>`, `>=` (等于, 不等于, 小于, 小于等于, 大于, 大于等于)。
    * 结果为 `LOGICAL` 类型。
* **逻辑运算符**:
    * `.NOT.`, `.AND.`, `.OR.`, `.EQV.`, `.NEQV.` (非, 与, 或, 等价, 不等价/异或)。
    * 操作 `LOGICAL` 值。
* **运算符优先级**: 有固定的运算顺序（如乘方 > 乘除 > 加减 > 关系 > 逻辑）。
* **建议**: **大量使用圆括号 `()`** 来明确运算顺序，避免混淆。

**示例:**

```fortran
PROGRAM OperatorDemo
  IMPLICIT NONE
  INTEGER :: a = 7, b = 2
  REAL :: x = 7.0, y = 2.0
  LOGICAL :: p = .TRUE., q = .FALSE.

  PRINT *, "7 / 2 =", a / b            ! 输出 3 (整数除法)
  PRINT *, "7.0 / 2.0 =", x / y        ! 输出 3.5 (实数除法)
  PRINT *, "7 > 2 is", a > b          ! 输出 .TRUE.
  PRINT *, ".TRUE. .AND. .FALSE. is", p .AND. q ! 输出 .FALSE.
  PRINT *, "5.0 + 2.0 * 3.0 =", 5.0 + 2.0 * 3.0 ! 输出 11.0 (乘法优先)
  PRINT *, "(5.0 + 2.0) * 3.0 =", (5.0 + 2.0) * 3.0 ! 输出 21.0 (括号优先)
END PROGRAM OperatorDemo
```
---