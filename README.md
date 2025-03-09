\documentclass{book}
\usepackage{graphicx}
\usepackage{lipsum} % For placeholder text, remove in final draft
\usepackage{hyperref}
\usepackage{listings}
\usepackage{array}
\usepackage{geometry}

\title{Fortran Hawatri}
\author{Kia Hawatri}


\begin{document}
\maketitle
\chapter*{Introduction to Fortran 77}

Fortran, short for \textit{Formula Translation}, is one of the oldest high-level programming languages, with its origins dating back to the 1950s. Developed by IBM for scientific and engineering applications, Fortran revolutionized the way numerical computations were performed, enabling researchers and engineers to write programs that were both efficient and portable. Fortran 77, released in 1978, is one of the most influential versions of the language, introducing structured programming features while retaining the simplicity and power that made Fortran a cornerstone of computational science.

\section*{Why Fortran 77?}
Fortran 77 represents a significant milestone in the evolution of programming languages. It introduced many features that are now considered standard in modern programming, such as structured control constructs (\texttt{IF-THEN-ELSE}, \texttt{DO} loops), character string handling, and improved input/output capabilities. Despite its age, Fortran 77 remains relevant today, particularly in legacy systems and fields such as computational physics, climate modeling, and engineering simulations. Its straightforward syntax and focus on numerical computation make it an excellent language for beginners and a powerful tool for experts.

\section*{Who Is This Book For?}
This book is designed for anyone interested in learning Fortran 77, whether you are a student, a researcher, or a professional in a technical field. No prior programming experience is required, as we will start from the basics and gradually build up to more advanced topics. For those already familiar with other programming languages, this book will help you quickly adapt to Fortran's unique features and conventions. By the end of this book, you will have a solid understanding of Fortran 77 and be able to write, debug, and optimize your own programs.

\section*{What Will You Learn?}
In this book, we will cover the following topics:
\begin{itemize}
    \item The history and evolution of Fortran.
    \item Basic syntax and data types in Fortran 77.
    \item Control structures and loops.
    \item Arrays and subroutines.
    \item Input/output operations and file handling.
    \item Common pitfalls and best practices.
    \item Applications of Fortran 77 in scientific computing.
\end{itemize}

\section*{How to Use This Book}
Each chapter is designed to build on the previous one, with clear explanations, practical examples, and exercises to reinforce your understanding. Code snippets are provided throughout the text, and complete programs are available for download from the book's companion website. Whether you are reading this book cover-to-cover or using it as a reference, we encourage you to experiment with the examples and write your own programs to solidify your knowledge.

\section*{A Legacy of Innovation}
Fortran 77 may be a product of its time, but its influence is timeless. By learning Fortran 77, you are not only gaining a valuable skill but also connecting with a rich history of innovation in computing. As you progress through this book, you will discover why Fortran remains a trusted tool for solving some of the world's most complex problems. Welcome to the world of Fortran 77—let's begin this journey together.

\chapter{Your First Fortran 77 Program}

\section*{Writing "Hello, World!" in Fortran 77}
Let's start with the classic first program. Create a file named \texttt{hello.f} and type the following:

\begin{verbatim}
C     FORTRAN 77 HELLO WORLD PROGRAM
      PROGRAM HELLOW
C     THIS IS A COMMENT LINE
      WRITE(*,*) 'HELLO WORLD'
      END
\end{verbatim}

\subsection*{Explanation of the Code}
\begin{itemize}
    \item Line 1: Comment line starting with 'C' in column 1
    \item Line 2: \texttt{PROGRAM HELLOW} declares the main program
    \item Line 3: Another comment line
    \item Line 4: \texttt{WRITE(*,*)} outputs text 
    \item Line 5: \texttt{END} marks the program's conclusion
\end{itemize}

\section*{Fortran 77 Coding Rules}

\subsection*{Fixed-Form Formatting}
Fortran 77 uses \textbf{fixed-form source code} with strict column rules:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\textbf{Columns} & \textbf{Purpose} \\ 
\hline
1-5 & Statement labels, \texttt{FORMAT} identifiers \\
6   & Continuation marker (any character except '0' or space) \\
7-72 & Program statements \\
73+ & Ignored (historical 80-column punch card limit) \\
\hline
\end{tabular}
\end{center}

\subsection*{Key Syntax Rules}
\begin{itemize}
    \item \textbf{Comments}: Start with 'C', '*', or '!' in column 1
    \item \textbf{Continuation}: Place a character in column 6 to continue long lines
    \item \textbf{Labels}: Numeric identifiers (1-99999) in columns 1-5
    \item \textbf{Statements}: Begin in column 7 or later
    \item \textbf{Case Insensitive}: \texttt{WRITE}, \texttt{Write}, and \texttt{write} are equivalent
\end{itemize}

\section*{Spacing Requirements Explained}

\subsection*{Column Layout Example}
\begin{verbatim}
123456789...
C Comment line
     PROGRAM TEST
     WRITE(*,*) 'THIS IS A
    * CONTINUED LINE'
     X = 5.0
     IF (X .GT. 0) THEN
         Y = X**2
     ENDIF
     END
\end{verbatim}

\begin{itemize}
    \item Line 1: Comment (C in column 1)
    \item Line 2: Program starts in column 7
    \item Line 3: Full statement in columns 7-72
    \item Line 4: Continuation character (*) in column 6
    \item Line 7: Code indentation (optional but recommended)
\end{itemize}

\subsection*{Why These Rules Exist?}
The column-based format dates back to punch card era programming:
\begin{itemize}
    \item Columns 1-5: Used for card sequence numbers
    \item Column 6: Continuation indicator for multi-card statements
    \item Columns 73-80: Originally used for card identification numbers
\end{itemize}

\section*{Common Pitfalls to Avoid}
\begin{itemize}
    \item Starting code in column 6 (reserved for continuation)
    \item Using lowercase letters (allowed but not traditional)
    \item Forgetting the continuation marker for long lines
    \item Writing past column 72 (code will be truncated)
    \item Mixing tabs and spaces (use spaces only)
\end{itemize}

\subsection*{Best Practices}
\begin{itemize}
    \item Use uppercase letters for Fortran keywords
    \item Indent code blocks for readability (columns 7-72)
    \item Use comment headers for major sections
    \item Always include \texttt{IMPLICIT NONE} (more on this later)
    \item Test line length with a ruler in your editor
\end{itemize}

\section*{Compiling Your First Program}
Use a Fortran 77 compiler like \texttt{gfortran}:
\begin{verbatim}
gfortran -std=legacy hello.f -o hello
./hello
\end{verbatim}
Output should be: \texttt{HELLO, WORLD!}

\section{Commenting in Fortran 77}

\subsection*{The Art of Documentation}
Comments are essential for writing maintainable code, especially in Fortran 77 where the fixed-format syntax can appear cryptic to modern programmers. Proper commenting helps explain complex algorithms, document assumptions, and make code accessible to future readers.

\subsection*{Comment Syntax}
Fortran 77 has strict rules for comments:
\begin{itemize}
    \item Any line with \texttt{C}, \texttt{*}, or \texttt{!} in \textbf{column 1} becomes a comment
    \item Entire line is ignored by the compiler
    \item No inline comments (unlike modern languages)
    \item Blank lines are allowed but not considered comments
\end{itemize}

\begin{verbatim}
C THIS IS A CLASSIC FORTRAN COMMENT
* THIS VARIANT IS OFTEN USED FOR HEADERS
! SOME COMPILERS SUPPORT THIS (NON-STANDARD)
\end{verbatim}

\subsection*{Effective Commenting Techniques}

\subsubsection*{Basic Example}
\begin{verbatim}
C     ============================================
C     PROGRAM: FLUID_SIMULATION
C     PURPOSE: SOLVE NAVIER-STOKES EQUATIONS
C     AUTHOR:  J. DOE
C     DATE:    2023-08-20
C     ============================================
      PROGRAM FLUID
C     DECLARE VARIABLES
      REAL U(100), V(100), P(100)
C     INITIALIZE ARRAYS
      DO 10 I = 1,100
        U(I) = 0.0
        V(I) = 0.0
10    CONTINUE
*     MAIN SIMULATION LOOP
      DO 20 T = 1,1000
C       UPDATE PRESSURE FIELD
        CALL CALC_PRESSURE(P,U,V)
20    CONTINUE
      END
\end{verbatim}

\subsection*{Commenting Best Practices}
\begin{itemize}
    \item \textbf{Header Blocks}: Use comments at the start of programs/subroutines to describe:
    \begin{itemize}
        \item Program purpose
        \item Input/Output specifications
        \item Author and revision history
        \item Special algorithms used
    \end{itemize}
    
    \item \textbf{Section Dividers}:
    \begin{verbatim}
C     ---- INITIALIZATION PHASE ----
    \end{verbatim}
    
    \item \textbf{Explanatory Comments}:
    \begin{verbatim}
C     APPLY COOLEY-TUKEY FFT ALGORITHM HERE
C     NOTE: ARRAY INDICES START AT 1 PER FORTRAN CONVENTION
    \end{verbatim}
    
    \item \textbf{Warnings}:
    \begin{verbatim}
C     WARNING: DON'T CALL THIS SUBROUTINE RECURSIVELY
C     GLOBAL VARIABLE X MODIFIED IN SECTION 3.2
    \end{verbatim}
\end{itemize}

\subsection*{Common Commenting Mistakes}
\begin{itemize}
    \item \textbf{Improper Alignment}:
    \begin{verbatim}
      C THIS COMMENT WILL CAUSE ERROR (C NOT IN COLUMN 1)
    \end{verbatim}
    
    \item \textbf{Redundant Comments}:
    \begin{verbatim}
C     INCREMENT I
      I = I + 1  (BAD - OBVIOUS OPERATION)
    \end{verbatim}
    
    \item \textbf{Outdated Comments}:
    \begin{verbatim}
C     MAX ARRAY SIZE 50 (ACTUAL SIZE IS 100 IN CODE)
    \end{verbatim}
\end{itemize}

\subsection*{Advanced Commenting Strategies}

\subsubsection*{Commenting Large Blocks}
\begin{verbatim}
C     ================================================
C     SUBROUTINE: MATRIX_SOLVER
C     PURPOSE:    SOLVE LINEAR SYSTEM AX=B
C     METHOD:     GAUSSIAN ELIMINATION WITH PIVOTING
C     ARGUMENTS:
C       A - COEFFICIENT MATRIX (N x N)
C       B - RIGHT-HAND SIDE VECTOR (N)
C       X - SOLUTION VECTOR (OUTPUT)
C       N - SYSTEM DIMENSION
C     ================================================
      SUBROUTINE MATRIX_SOLVER(A,B,X,N)
      DIMENSION A(N,N), B(N), X(N)
C     ... implementation ...
      END
\end{verbatim}

\subsubsection*{Temporary Code Exclusion}
\begin{verbatim}
C     DEBUGGING CODE - DISABLE FOR PRODUCTION
CC    WRITE(*,*) 'CURRENT VALUE:', X
C     CALL DEBUG_ROUTINE
\end{verbatim}

\subsection*{Historical Context}
The column-based commenting system originated from:
\begin{itemize}
    \item Punch card era physical constraints
    \item Need for quick visual identification of comments
    \item Limited screen space on early text terminals
\end{itemize}

\subsection*{Modern Considerations}
While maintaining Fortran 77 compatibility:
\begin{itemize}
    \item Many modern editors support syntax highlighting
    \item Consider using lowercase for better readability:
    \begin{verbatim}
c     Mixed-case comments often read better
c     Than all-uppercase text blocks
    \end{verbatim}
    \item Use version control instead of comment-based revision tracking
\end{itemize}

\section{Variables in Fortran 77}

\subsection*{Variable Types}
Fortran 77 supports these fundamental data types:
\begin{center}
\begin{tabular}{|l|l|l|}
\hline
\textbf{Type} & \textbf{Description} & \textbf{Example Values} \\ 
\hline
\texttt{INTEGER} & Whole numbers & -3, 0, 42 \\
\texttt{REAL} & Single-precision floating point & 3.14, -0.001 \\
\texttt{DOUBLE PRECISION} & Double-precision floating point & 1.23456D+08 \\
\texttt{CHARACTER} & Text/String & 'Hello', 'A' \\
\texttt{LOGICAL} & Boolean values & \texttt{.TRUE.}, \texttt{.FALSE.} \\
\texttt{COMPLEX} & Complex numbers & (1.0, -2.5) \\
\hline
\end{tabular}
\end{center}

\subsection*{Declaration Syntax}
Variables must be declared at the start of the program/subroutine:
\begin{verbatim}
      PROGRAM VARIABLES
      INTEGER COUNT, INDEX
      REAL TEMP, PRESSURE
      CHARACTER*20 NAME
      LOGICAL FLAG
      DOUBLE PRECISION PI
      COMPLEX WAVE
\end{verbatim}

\subsection*{Naming Rules}
\begin{itemize}
    \item Maximum 6 characters (truncated if longer)
    \item Must start with a letter (A-Z)
    \item Subsequent characters: letters/digits (0-9)
    \item Case insensitive: \texttt{Var} = \texttt{VAR} = \texttt{var}
    \item Avoid reserved words: \texttt{PROGRAM}, \texttt{END}, etc.
\end{itemize}

\subsection*{Type-Specific Examples}

\subsubsection*{INTEGER}
\begin{verbatim}
      PROGRAM INT_EX
      INTEGER AGE, YEAR
      WRITE(*,*) 'ENTER BIRTH YEAR:'
      READ(*,*) YEAR
      AGE = 2023 - YEAR
      WRITE(*,*) 'AGE:', AGE
      STOP
      END
\end{verbatim}

\subsubsection*{REAL}
\begin{verbatim}
      PROGRAM REAL_EX
      REAL TEMP_C, TEMP_F
      WRITE(*,*) 'ENTER FAHRENHEIT TEMP:'
      READ(*,*) TEMP_F
      TEMP_C = (TEMP_F - 32.0) * 5.0/9.0
      WRITE(*,*) 'CELSIUS:', TEMP_C
      STOP
      END
\end{verbatim}

\subsubsection*{DOUBLE PRECISION}
\begin{verbatim}
      PROGRAM DOUBLE_EX
      DOUBLE PRECISION PI
      PI = 4.0D0 * ATAN(1.0D0)
      WRITE(*,*) 'PI =', PI
      STOP
      END
\end{verbatim}

\subsubsection*{CHARACTER}
\begin{verbatim}
      PROGRAM CHAR_EX
      CHARACTER*15 CITY
      WRITE(*,*) 'ENTER YOUR CITY:'
      READ(*,*) CITY
      WRITE(*,*) 'CITY:', CITY
      STOP
      END
\end{verbatim}

\subsubsection*{LOGICAL}
\begin{verbatim}
      PROGRAM LOG_EX
      LOGICAL FLAG
      FLAG = .TRUE.
      IF (FLAG) THEN
          WRITE(*,*) 'CONDITION IS TRUE'
      ENDIF
      STOP
      END
\end{verbatim}

\subsubsection*{COMPLEX}
\begin{verbatim}
      PROGRAM COMPLEX_EX
      COMPLEX Z
      Z = (3.0, 4.0)  ! 3 + 4i
      WRITE(*,*) 'MAGNITUDE:', ABS(Z)
      STOP
      END
\end{verbatim}

\subsection*{Type Conversion}
Convert between types explicitly:
\begin{verbatim}
      REAL X
      INTEGER N
      X = 3.14
      N = INT(X)      ! N becomes 3
      X = REAL(N)     ! X becomes 3.0
\end{verbatim}

\subsection*{Common Mistakes}
\begin{itemize}
    \item \textbf{Implicit Typing}: Variables starting with I-N are integers by default
    \begin{verbatim}
      K = 2.5  ! Becomes INTEGER 2 (no error!)
    \end{verbatim}
    
    \item \textbf{Solution}: Always declare \texttt{IMPLICIT NONE} first
    \begin{verbatim}
      PROGRAM SAFE
      IMPLICIT NONE
      \end{verbatim}
    
    \item \textbf{Truncation}:
    \begin{verbatim}
      CHARACTER*5 NAME = 'LONDON'  ! Becomes 'LONDO'
    \end{verbatim}
    
    \item \textbf{Precision Loss}:
    \begin{verbatim}
      REAL PI = 3.1415926535  ! Stored as 3.141593
    \end{verbatim}
\end{itemize}

\subsection*{Best Practices}
\begin{itemize}
    \item Always use \texttt{IMPLICIT NONE} to force declarations
    \item Choose meaningful names: \texttt{VOLTAGE} vs \texttt{V}
    \item Use \texttt{DOUBLE PRECISION} for scientific calculations
    \item Initialize variables before use
    \item Comment on variable purposes in complex programs
\end{itemize}

\subsection*{Storage Considerations}
\begin{center}
\begin{tabular}{|l|l|}
\hline
\textbf{Type} & \textbf{Typical Size} \\ 
\hline
\texttt{INTEGER} & 4 bytes \\
\texttt{REAL} & 4 bytes \\
\texttt{DOUBLE PRECISION} & 8 bytes \\
\texttt{CHARACTER*n} & n bytes \\
\texttt{LOGICAL} & 4 bytes (usually) \\
\texttt{COMPLEX} & 8 bytes (2×4-byte reals) \\
\hline
\end{tabular}
\end{center}

\section{User Input and Variable Handling}

\subsection*{Basic Input-Process-Output Workflow}
Fortran 77 programs typically follow this pattern:
\begin{enumerate}
    \item Prompt user with \texttt{WRITE(*,*)}
    \item Read input with \texttt{READ(*,*)}
    \item Process data
    \item Display results with \texttt{WRITE(*,*)}
\end{enumerate}

\subsection*{Single Variable Example}
\begin{verbatim}
C     PROGRAM: AGE_CHECKER
C     PURPOSE: DEMONSTRATE SINGLE VARIABLE INPUT
      PROGRAM AGE_CHECK
      INTEGER AGE
C     DISPLAY PROMPT
      WRITE(*,*) 'ENTER YOUR AGE:'
C     READ INTEGER INPUT
      READ(*,*) AGE
C     DISPLAY RESULT
      WRITE(*,*) 'IN 10 YEARS YOU WILL BE:', AGE + 10
      STOP
      END
\end{verbatim}

\subsection*{Multiple Variables Example}
\begin{verbatim}
C     PROGRAM: RECTANGLE_AREA
C     INPUT: LENGTH AND WIDTH
C     OUTPUT: CALCULATED AREA
      PROGRAM RECT_AREA
      REAL LENGTH, WIDTH, AREA
C     GET DIMENSIONS
      WRITE(*,*) 'ENTER LENGTH AND WIDTH (SEPARATE BY SPACE):'
      READ(*,*) LENGTH, WIDTH
C     CALCULATE AND DISPLAY
      AREA = LENGTH * WIDTH
      WRITE(*,*) 'AREA OF RECTANGLE:', AREA
      STOP
      END
\end{verbatim}

\subsection*{Type-Specific Input Handling}

\subsubsection*{Character Input}
\begin{verbatim}
C     PROGRAM: GREETER
C     DEMONSTRATES STRING HANDLING
      PROGRAM GREETER
      CHARACTER*20 NAME
C     GET USER NAME
      WRITE(*,*) 'ENTER YOUR NAME:'
      READ(*,*) NAME
C     DISPLAY GREETING
      WRITE(*,*) 'HELLO, ', TRIM(NAME), '! WELCOME!'
      STOP
      END
\end{verbatim}

\subsubsection*{Logical Input}
\begin{verbatim}
C     PROGRAM: LOGIC_TEST
C     SHOWS BOOLEAN INPUT HANDLING
      PROGRAM LOGTEST
      LOGICAL FLAG
C     GET TRUE/FALSE INPUT
      WRITE(*,*) 'ENTER .TRUE. OR .FALSE.:'
      READ(*,*) FLAG
C     DISPLAY NEGATION
      WRITE(*,*) 'NEGATED VALUE:', .NOT.FLAG
      STOP
      END
\end{verbatim}

\subsection*{Input Validation}
\begin{verbatim}
C     PROGRAM: TEMP_CONVERTER
C     WITH BASIC ERROR CHECKING
      PROGRAM TEMPCONV
      REAL FAHREN
C     INPUT LOOP
10    WRITE(*,*) 'ENTER TEMPERATURE (-200 TO 200 F):'
      READ(*,*) FAHREN
      IF (FAHREN .LT. -200 .OR. FAHREN .GT. 200) THEN
          WRITE(*,*) 'INVALID INPUT! TRY AGAIN.'
          GOTO 10
      ENDIF
C     CONVERT TO CELSIUS
      CELSIUS = (FAHREN - 32.0) * 5.0/9.0
      WRITE(*,*) 'CELSIUS TEMPERATURE:', CELSIUS
      STOP
      END
\end{verbatim}

\subsection*{Troubleshooting Input Issues}
\begin{center}
\begin{tabular}{|p{4cm}|p{8cm}|}
\hline
\textbf{Issue} & \textbf{Solution} \\ 
\hline
User enters text for numeric input & Program crashes - add error handling (see Ch. 7) \\
\hline
Multiple values without spaces & Use comma/space separation: \texttt{10,20} not \texttt{10 20} \\
\hline
String longer than declaration & Truncated to variable length \\
\hline
Mixing data types & Ensure \texttt{READ} matches variable types \\
\hline
\end{tabular}
\end{center}

\subsection*{Best Practices}
\begin{itemize}
    \item Always include clear prompts before \texttt{READ} statements
    \item Use descriptive variable names
    \item Initialize variables before use
    \item Add comments explaining non-obvious input requirements
    \item Test with boundary values and invalid inputs
    \item Use \texttt{TRIM()} for character variables in output
\end{itemize}

\subsection*{Complete Example with Comments}
\begin{verbatim}
C     PROGRAM: EMPLOYEE_RECORD
C     PURPOSE: DEMONSTRATE MIXED DATA TYPE INPUT
      PROGRAM EMP_REC
      CHARACTER*15 NAME
      INTEGER AGE
      REAL SALARY
      LOGICAL FULLTIME

C     GET EMPLOYEE DETAILS
      WRITE(*,*) 'ENTER EMPLOYEE NAME:'
      READ(*,*) NAME
      WRITE(*,*) 'ENTER AGE (YEARS):'
      READ(*,*) AGE
      WRITE(*,*) 'ENTER ANNUAL SALARY:'
      READ(*,*) SALARY
      WRITE(*,*) 'FULL-TIME? (.TRUE./.FALSE.):'
      READ(*,*) FULLTIME

C     DISPLAY SUMMARY
      WRITE(*,*) 'EMPLOYEE DETAILS:'
      WRITE(*,*) 'NAME:    ', TRIM(NAME)
      WRITE(*,*) 'AGE:     ', AGE
      WRITE(*,*) 'SALARY:  $', SALARY
      WRITE(*,*) 'FULL-TIME: ', FULLTIME

      STOP
      END
\end{verbatim}

\subsection*{Notes on Input Formatting}
\begin{itemize}
    \item Use free-format \texttt{READ(*,*)} for simple programs
    \item Numeric input accepts:
    \begin{itemize}
        \item Integers: \texttt{42}, \texttt{-15}
        \item Reals: \texttt{3.14}, \texttt{.5}, \texttt{6.02E23}
    \end{itemize}
    \item Logical input requires \texttt{.TRUE.} or \texttt{.FALSE.}
    \item Character input stops at first whitespace (use \texttt{READ} with format for spaces)
\end{itemize}

\subsection*{Compilation \& Testing Tip}
\begin{verbatim}
# Compile with strict Fortran 77 checking
gfortran -std=legacy -Wall input_example.f -o demo
\end{verbatim}

\section{Arithmetic Operations in Fortran 77}

\subsection*{Fundamental Arithmetic Operators}
Fortran 77 supports standard mathematical operations with this precedence:
\begin{center}
\begin{tabular}{|l|l|l|}
\hline
\textbf{Operator} & \textbf{Operation} & \textbf{Example} \\ 
\hline
\texttt{**} & Exponentiation & \texttt{X**2} \\
\texttt{*} & Multiplication & \texttt{A * B} \\
\texttt{/} & Division & \texttt{Y / Z} \\
\texttt{+} & Addition & \texttt{C + D} \\
\texttt{-} & Subtraction & \texttt{M - N} \\
\hline
\end{tabular}
\end{center}

\subsection*{Basic Operation Examples}

\subsubsection*{Simple Calculations}
\begin{verbatim}
C     PROGRAM: BASIC_MATH
C     DEMONSTRATES FUNDAMENTAL OPERATIONS
      PROGRAM CALC
      REAL X, Y, RESULT
      
      X = 10.0
      Y = 3.0
      
      RESULT = X + Y
      WRITE(*,*) 'SUM:      ', RESULT
      
      RESULT = X - Y
      WRITE(*,*) 'DIFFERENCE:', RESULT
      
      RESULT = X * Y
      WRITE(*,*) 'PRODUCT:  ', RESULT
      
      RESULT = X / Y
      WRITE(*,*) 'QUOTIENT: ', RESULT
      
      RESULT = X**2 + Y**3
      WRITE(*,*) 'X² + Y³:  ', RESULT
      
      STOP
      END
\end{verbatim}

\subsection*{Operator Precedence}
Operations follow PEMDAS rules (Parentheses, Exponents, Multiplication/Division, Addition/Subtraction):

\begin{verbatim}
C     PROGRAM: PRECEDENCE
C     SHOWS ORDER OF OPERATIONS
      PROGRAM ORDER
      REAL A, B, C, RESULT
      
      A = 2.0
      B = 3.0
      C = 4.0
      
C     EQUIVALENT TO: (A + B) * C
      RESULT = A + B * C
      WRITE(*,*) 'WITHOUT PARENTHESES:', RESULT
      
C     EXPLICIT ORDERING
      RESULT = (A + B) * C
      WRITE(*,*) 'WITH PARENTHESES:   ', RESULT
      
      STOP
      END
\end{verbatim}

\subsection*{Mixed-Type Operations}
Fortran automatically converts types during operations:

\begin{verbatim}
C     PROGRAM: TYPE_MIX
C     DEMONSTRATES INTEGER/REAL INTERACTIONS
      PROGRAM TYPEMIX
      INTEGER I
      REAL R
      DOUBLE PRECISION D
      
      I = 5
      R = 2.5
      D = 1.0D0
      
C     INTEGER + REAL = REAL
      WRITE(*,*) '5 + 2.5 =', I + R
      
C     REAL / INTEGER = REAL
      WRITE(*,*) '2.5 / 2 =', R / 2
      
C     DOUBLE PRECISION OPERATION
      D = D / 3.0D0
      WRITE(*,*) '1/3 (DP):', D
      
      STOP
      END
\end{verbatim}

\subsection*{Common Mathematical Functions}
Fortran 77 provides intrinsic functions:

\begin{verbatim}
C     PROGRAM: MATH_FUNCS
C     SHOWS BUILT-IN MATHEMATICAL FUNCTIONS
      PROGRAM MFUNCS
      REAL X, Y, ANGLE
      
      X = 16.0
      Y = 2.5
      ANGLE = 45.0
      
C     SQUARE ROOT
      WRITE(*,*) 'SQRT(16):    ', SQRT(X)
      
C     EXPONENTIAL
      WRITE(*,*) 'EXP(2.5):    ', EXP(Y)
      
C     NATURAL LOG
      WRITE(*,*) 'LOG(2.5):    ', LOG(Y)
      
C     TRIG FUNCTIONS (IN RADIANS)
      WRITE(*,*) 'SIN(45°):    ', SIN(ANGLE * 3.14159 / 180.0)
      
C     ABSOLUTE VALUE
      WRITE(*,*) 'ABS(-2.5):   ', ABS(-Y)
      
C     MODULO OPERATION
      WRITE(*,*) 'MOD(17,5):   ', MOD(17, 5)
      
      STOP
      END
\end{verbatim}

\subsection*{Complete Example: Quadratic Equation}
\begin{verbatim}
C     PROGRAM: QUADRATIC_SOLVER
C     SOLVES AX² + BX + C = 0
      PROGRAM QUAD
      REAL A, B, C, DISC, X1, X2
      
C     GET COEFFICIENTS
      WRITE(*,*) 'ENTER A, B, C (SEPARATED BY SPACES):'
      READ(*,*) A, B, C
      
C     CALCULATE DISCRIMINANT
      DISC = B**2 - 4.0*A*C
      
C     HANDLE COMPLEX ROOTS
      IF (DISC .LT. 0.0) THEN
          WRITE(*,*) 'COMPLEX ROOTS!'
          STOP
      ENDIF
      
C     CALCULATE ROOTS
      X1 = (-B + SQRT(DISC)) / (2.0*A)
      X2 = (-B - SQRT(DISC)) / (2.0*A)
      
      WRITE(*,*) 'ROOTS ARE:', X1, 'AND', X2
      STOP
      END
\end{verbatim}

\subsection*{Common Arithmetic Pitfalls}
\begin{center}
\begin{tabular}{|p{4cm}|p{8cm}|}
\hline
\textbf{Issue} & \textbf{Solution} \\ 
\hline
Integer division: \texttt{5/2 = 2} & Use real numbers: \texttt{5.0/2.0 = 2.5} \\
\hline
Overflow with large exponents & Use \texttt{DOUBLE PRECISION} variables \\
\hline
Division by zero & Add validation checks before division \\
\hline
Mixing precedence & Use parentheses for clarity \\
\hline
\end{tabular}
\end{center}

\subsection*{Best Practices}
\begin{itemize}
    \item Use parentheses for complex expressions
    \item Avoid integer division when fractional results are needed
    \item Use \texttt{DOUBLE PRECISION} for sensitive calculations
    \item Check for division by zero and negative roots
    \item Use meaningful variable names (\texttt{VOLUME} vs \texttt{V})
\end{itemize}

\subsection*{Troubleshooting Table}
\begin{center}
\begin{tabular}{|p{4cm}|p{8cm}|}
\hline
\textbf{Error Message} & \textbf{Meaning} \\ 
\hline
\texttt{Arithmetic overflow} & Result exceeds variable type capacity \\
\hline
\texttt{Divided by zero} & Attempted division with zero denominator \\
\hline
\texttt{Type mismatch} & Mixed incompatible types without conversion \\
\hline
\end{tabular}
\end{center}

\subsection*{Compilation Note}
\begin{verbatim}
# Enable all warnings for arithmetic checks
gfortran -std=legacy -Wall -Wextra math_example.f -o demo
\end{verbatim}

\section{Type Conversion in Fortran 77}

\subsection*{Implicit vs. Explicit Conversion}
Fortran 77 allows both implicit (automatic) and explicit (programmer-controlled) type conversion. While convenient, implicit conversion can lead to subtle bugs, making explicit conversion the safer approach.

\subsection*{Implicit Type Conversion}
\begin{itemize}
    \item \textbf{Mixed-Type Operations}: Fortran automatically promotes types in expressions
    \begin{verbatim}
      INTEGER I = 5
      REAL R = 2.5
      RESULT = I + R  ! I is converted to REAL (5.0) first
    \end{verbatim}
    
    \item \textbf{Assignment Conversion}: Right-hand side converted to left-hand side type
    \begin{verbatim}
      REAL X
      X = 3   ! Integer 3 converted to REAL 3.0
    \end{verbatim}
    
    \item \textbf{Default Typing}: Variables starting with I-N are INTEGER by default
    \begin{verbatim}
      K = 2.7  ! K is INTEGER → becomes 2 (truncation occurs)
    \end{verbatim}
\end{itemize}

\subsection*{Explicit Type Conversion Functions}
Fortran provides intrinsic functions for controlled conversion:

\begin{center}
\begin{tabular}{|l|l|}
\hline
\textbf{Function} & \textbf{Purpose} \\ 
\hline
\texttt{INT(X)} & Convert to \texttt{INTEGER} (truncates) \\
\texttt{REAL(X)} & Convert to single-precision \texttt{REAL} \\
\texttt{DBLE(X)} & Convert to \texttt{DOUBLE PRECISION} \\
\texttt{CMPLX(X,Y)} & Create \texttt{COMPLEX} number (X + Yi) \\
\texttt{ICHAR(C)} & Convert character to ASCII code \\
\texttt{CHAR(I)} & Convert ASCII code to character \\
\hline
\end{tabular}
\end{center}

\subsection*{Code Examples}

\subsubsection*{Integer to Real}
\begin{verbatim}
      PROGRAM INT2REAL
      INTEGER COUNT
      REAL AVERAGE
      COUNT = 7
C     EXPLICIT CONVERSION TO AVOID INTEGER DIVISION
      AVERAGE = REAL(COUNT) / 2.0
      WRITE(*,*) 'AVERAGE:', AVERAGE  ! Output: 3.5
      STOP
      END
\end{verbatim}

\subsubsection*{Real to Integer}
\begin{verbatim}
      PROGRAM REAL2INT
      REAL TEMP = 98.6
      INTEGER ITEMP
C     TRUNCATE DECIMAL PART
      ITEMP = INT(TEMP)
      WRITE(*,*) 'INTEGER TEMP:', ITEMP  ! Output: 98
      STOP
      END
\end{verbatim}

\subsubsection*{Double Precision Conversion}
\begin{verbatim}
      PROGRAM DBLE_CONV
      REAL PI_SINGLE = 3.14159
      DOUBLE PRECISION PI_DOUBLE
C     PRESERVE PRECISION
      PI_DOUBLE = DBLE(PI_SINGLE)
      WRITE(*,*) 'DOUBLE PI:', PI_DOUBLE
      STOP
      END
\end{verbatim}

\subsection*{Character Conversions}
\begin{verbatim}
      PROGRAM CHAR_CONV
      CHARACTER C
      INTEGER ASCII
C     CHARACTER TO ASCII
      C = 'A'
      ASCII = ICHAR(C)
      WRITE(*,*) 'ASCII CODE:', ASCII  ! Output: 65
      
C     ASCII TO CHARACTER
      C = CHAR(66)
      WRITE(*,*) 'CHARACTER:', C  ! Output: B
      STOP
      END
\end{verbatim}

\subsection*{Common Pitfalls}
\begin{center}
\begin{tabular}{|p{4cm}|p{8cm}|}
\hline
\textbf{Issue} & \textbf{Solution} \\ 
\hline
\texttt{REAL(5/2) = 2.0} (integer division first) & Use \texttt{REAL(5)/2.0 = 2.5} \\
\hline
\texttt{INT(3.999) = 3} (truncation) & Use \texttt{NINT()} for rounding \\
\hline
Implicit real→integer conversion & Always use \texttt{INT()} explicitly \\
\hline
Precision loss in real→double & Use \texttt{DBLE()} on literals: \texttt{DBLE(0.1D0)} \\
\hline
\end{tabular}
\end{center}

\subsection*{Best Practices}
\begin{itemize}
    \item Always use \texttt{IMPLICIT NONE} to disable automatic typing
    \item Perform explicit conversions for clarity
    \item Use \texttt{NINT()} instead of \texttt{INT()} for rounding
    \item Avoid mixing types in complex expressions
    \item Comment non-obvious conversions
\end{itemize}

\subsection*{Advanced Conversion: Complex Numbers}
\begin{verbatim}
      PROGRAM COMPLEX_CONV
      COMPLEX Z
      REAL X, Y
      X = 3.0
      Y = 4.0
C     CREATE COMPLEX FROM REALS
      Z = CMPLX(X, Y)
      WRITE(*,*) 'COMPLEX:', Z  ! Output: (3.0,4.0)
      STOP
      END
\end{verbatim}

\subsection*{Type Conversion Rules}
\begin{center}
\begin{tabular}{|l|l|}
\hline
\textbf{Conversion} & \textbf{Behavior} \\ 
\hline
\texttt{REAL → INTEGER} & Truncates decimal (no rounding) \\
\texttt{INTEGER → REAL} & Exact conversion \\
\texttt{REAL → DOUBLE} & Preserves precision \\
\texttt{DOUBLE → REAL} & Truncates to single precision \\
\texttt{CHARACTER → INTEGER} & ASCII code conversion \\
\hline
\end{tabular}
\end{center}

\subsection*{Why Explicit Conversion Matters}
\begin{verbatim}
C DANGEROUS IMPLICIT CONVERSION EXAMPLE
      PROGRAM DANGER
      IMPLICIT NONE
      REAL A = 5.0
      INTEGER B = 2
      WRITE(*,*) A/B  ! = 2.5 (GOOD)
      
      REAL C = 5
      INTEGER D = 2
      WRITE(*,*) C/D  ! = 2.5 (STILL GOOD? DEPENDS ON COMPILER!)
      STOP
      END
\end{verbatim}

\subsection*{Final Recommendations}
\begin{itemize}
    \item Use \texttt{REAL()} when mixing integers and reals
    \item Prefer \texttt{DBLE()} for high-precision calculations
    \item Always validate ranges before narrowing conversions
    \item Test conversions at boundary values
\end{itemize}

\section{Exercises}

\subsection*{Problem 1: Basic Program Structure}
Write a Fortran 77 program that:
\begin{itemize}
    \item Prints "MY FIRST FORTRAN PROGRAM"
    \item Includes proper comments
    \item Follows fixed-format rules
\end{itemize}
\textit{Sample Output:}
\begin{verbatim}
MY FIRST FORTRAN PROGRAM
\end{verbatim}

\subsection*{Problem 2: Variable Declaration}
Create a program that:
\begin{itemize}
    \item Declares an integer (AGE = 25)
    \item Declares a real number (PI = 3.14159)
    \item Declares a character (INITIAL = 'A')
    \item Prints all variables with labels
\end{itemize}

\subsection*{Problem 3: User Input Handling}
Write a program that:
\begin{itemize}
    \item Asks for user's name and birth year
    \item Calculates approximate age
    \item Prints formatted message
\end{itemize}
\textit{Sample Input/Output:}
\begin{verbatim}
ENTER YOUR NAME: JOHN
ENTER BIRTH YEAR: 1998
HELLO JOHN, YOU ARE ABOUT 25 YEARS OLD.
\end{verbatim}

\subsection*{Problem 4: Arithmetic Operations}
Create a program to calculate kinetic energy:
\[ KE = \frac{1}{2}mv^2 \]
Where:
\begin{itemize}
    \item Mass (m) = 10.5 kg
    \item Velocity (v) = 5.2 m/s
    \item Print result with description
\end{itemize}

\subsection*{Problem 5: Mixed-Type Calculation}
Write a program that:
\begin{itemize}
    \item Declares integer HOURS = 8
    \item Declares real RATE = 12.50
    \item Calculates total pay (HOURS * RATE)
    \item Explain why result is real
\end{itemize}

\subsection*{Problem 6: Explicit Type Conversion}
Create a program that:
\begin{itemize}
    \item Takes a real number input (e.g., 7.89)
    \item Converts to integer using INT()
    \item Converts to nearest integer using NINT()
    \item Prints both results
\end{itemize}

\subsection*{Problem 7: Temperature Conversion}
Write a program that:
\begin{itemize}
    \item Reads Celsius temperature
    \item Converts to Fahrenheit using:
    \[ F = \frac{9}{5}C + 32 \]
    \item Prints both temperatures
\end{itemize}

\subsection*{Problem 8: Geometric Calculations}
Develop a program to calculate:
\begin{itemize}
    \item Circle circumference \( C = 2\pi r \)
    \item Sphere volume \( V = \frac{4}{3}\pi r^3 \)
    \item Use radius = 5.0
    \item Print both results
\end{itemize}

\subsection*{Problem 9: Character Manipulation}
Create a program that:
\begin{itemize}
    \item Takes a character input
    \item Prints its ASCII code
    \item Takes an integer input (65-90)
    \item Prints corresponding character
\end{itemize}

\subsection*{Problem 10: Precision Demonstration}
Write a program that:
\begin{itemize}
    \item Calculates \( \frac{1}{3} \) as REAL
    \item Calculates \( \frac{1}{3} \) as DOUBLE PRECISION
    \item Prints both results
    \item Explain the difference
\end{itemize}

\subsection*{Challenge Problem: Unit Converter}
Create an interactive program that:
\begin{itemize}
    \item Asks user for length in kilometers
    \item Converts to miles (1 km = 0.621371 miles)
    \item Prints both values
    \item Uses proper type conversions
\end{itemize}
\textit{Bonus: Add error checking for negative inputs}

\section{Exercise Answers}

\subsection*{Problem 1: Basic Program Structure}
\begin{verbatim}
C     PROBLEM 1 SOLUTION
C     PURPOSE: DEMONSTRATE BASIC PROGRAM STRUCTURE
      PROGRAM FIRST
C     OUTPUT MESSAGE
      WRITE(*,*) 'MY FIRST FORTRAN PROGRAM'
      STOP
      END
\end{verbatim}
\textbf{Explanation:}
\begin{itemize}
    \item Comments start with 'C' in column 1
    \item Program statement begins in column 7
    \item WRITE statement uses list-directed output
    \item STOP terminates execution, END concludes program
\end{itemize}

\subsection*{Problem 2: Variable Declaration}
\begin{verbatim}
C     PROBLEM 2 SOLUTION
      PROGRAM VARDEC
      INTEGER AGE
      REAL PI
      CHARACTER INITIAL
C     INITIALIZE VALUES
      AGE = 25
      PI = 3.14159
      INITIAL = 'A'
C     OUTPUT RESULTS
      WRITE(*,*) 'AGE:    ', AGE
      WRITE(*,*) 'PI:     ', PI
      WRITE(*,*) 'INITIAL:', INITIAL
      STOP
      END
\end{verbatim}
\textbf{Key Points:}
\begin{itemize}
    \item Variables declared before executable statements
    \item Different data types require specific declarations
    \item Character literals enclosed in single quotes
\end{itemize}

\subsection*{Problem 3: User Input Handling}
\begin{verbatim}
C     PROBLEM 3 SOLUTION
      PROGRAM AGE_CALC
      CHARACTER*20 NAME
      INTEGER B_YEAR, AGE
C     GET INPUT
      WRITE(*,*) 'ENTER YOUR NAME:'
      READ(*,*) NAME
      WRITE(*,*) 'ENTER BIRTH YEAR:'
      READ(*,*) B_YEAR
C     CALCULATE AGE
      AGE = 2023 - B_YEAR
C     OUTPUT RESULTS
      WRITE(*,*) 'HELLO ', TRIM(NAME), ', YOU ARE ABOUT ', AGE, ' YEARS OLD.'
      STOP
      END
\end{verbatim}
\textbf{Notes:}
\begin{itemize}
    \item CHARACTER*20 reserves 20 characters for the name
    \item TRIM() removes trailing spaces from the name
    \item Input order must match variable types
\end{itemize}

\subsection*{Problem 4: Arithmetic Operations}
\begin{verbatim}
C     PROBLEM 4 SOLUTION
      PROGRAM KINETIC
      REAL MASS, VEL, KE
C     INITIALIZE VALUES
      MASS = 10.5
      VEL = 5.2
C     CALCULATE KINETIC ENERGY
      KE = 0.5 * MASS * VEL**2
C     OUTPUT RESULT
      WRITE(*,*) 'KINETIC ENERGY:', KE, 'JOULES'
      STOP
      END
\end{verbatim}
\textbf{Formula Implementation:}
\[ KE = \frac{1}{2} \times 10.5 \times (5.2)^2 \]
\begin{itemize}
    \item Exponentiation operator ** used for velocity squared
    \item Operator precedence handled correctly
\end{itemize}

\subsection*{Problem 5: Mixed-Type Calculation}
\begin{verbatim}
C     PROBLEM 5 SOLUTION
      PROGRAM PAYCALC
      INTEGER HOURS
      REAL RATE, TOTAL
C     INITIALIZE VALUES
      HOURS = 8
      RATE = 12.50
C     CALCULATE PAY
      TOTAL = HOURS * RATE
C     OUTPUT RESULT
      WRITE(*,*) 'TOTAL PAY: $', TOTAL
      STOP
      END
\end{verbatim}
\textbf{Type Conversion:}
\begin{itemize}
    \item Integer HOURS promoted to real during multiplication
    \item Result TOTAL is real (100.0 instead of 100)
    \item Explicit conversion not needed but recommended
\end{itemize}

\subsection*{Problem 6: Explicit Type Conversion}
\begin{verbatim}
C     PROBLEM 6 SOLUTION
      PROGRAM CONVERT
      REAL NUM
      INTEGER ITRUNC, IROUND
C     GET INPUT
      WRITE(*,*) 'ENTER A REAL NUMBER:'
      READ(*,*) NUM
C     CONVERT
      ITRUNC = INT(NUM)
      IROUND = NINT(NUM)
C     OUTPUT RESULTS
      WRITE(*,*) 'TRUNCATED:', ITRUNC
      WRITE(*,*) 'ROUNDED:  ', IROUND
      STOP
      END
\end{verbatim}
\textbf{Differences:}
\begin{itemize}
    \item INT(7.89) → 7 (truncation)
    \item NINT(7.89) → 8 (rounding)
    \item Always use NINT() for proper rounding
\end{itemize}

\subsection*{Problem 7: Temperature Conversion}
\begin{verbatim}
C     PROBLEM 7 SOLUTION
      PROGRAM TEMPCONV
      REAL CELS, FAHR
C     GET INPUT
      WRITE(*,*) 'ENTER TEMPERATURE IN CELSIUS:'
      READ(*,*) CELS
C     CONVERT
      FAHR = (9.0/5.0)*CELS + 32.0
C     OUTPUT RESULTS
      WRITE(*,*) CELS, 'C =', FAHR, 'F'
      STOP
      END
\end{verbatim}
\textbf{Formula Notes:}
\begin{itemize}
    \item Use 9.0/5.0 instead of 9/5 to force real division
    \item Operator precedence handled with parentheses
\end{itemize}

\subsection*{Problem 8: Geometric Calculations}
\begin{verbatim}
C     PROBLEM 8 SOLUTION
      PROGRAM GEOMETRY
      REAL R, CIRCUM, VOLUME
      PARAMETER (PI = 3.14159)
C     INITIALIZE RADIUS
      R = 5.0
C     CALCULATIONS
      CIRCUM = 2 * PI * R
      VOLUME = (4.0/3.0) * PI * R**3
C     OUTPUT
      WRITE(*,*) 'CIRCUMFERENCE:', CIRCUM
      WRITE(*,*) 'VOLUME:       ', VOLUME
      STOP
      END
\end{verbatim}
\textbf{Important:}
\begin{itemize}
    \item PARAMETER for constant PI
    \item Use parentheses for fractional coefficients
    \item R**3 calculates radius cubed
\end{itemize}

\subsection*{Problem 9: Character Manipulation}
\begin{verbatim}
C     PROBLEM 9 SOLUTION
      PROGRAM CHAR_CONVERT
      CHARACTER C
      INTEGER ASCII, CODE
C     CHARACTER TO ASCII
      WRITE(*,*) 'ENTER A CHARACTER:'
      READ(*,*) C
      ASCII = ICHAR(C)
      WRITE(*,*) 'ASCII CODE:', ASCII
C     ASCII TO CHARACTER
      WRITE(*,*) 'ENTER ASCII CODE (65-90):'
      READ(*,*) CODE
      C = CHAR(CODE)
      WRITE(*,*) 'CHARACTER:', C
      STOP
      END
\end{verbatim}
\textbf{Notes:}
\begin{itemize}
    \item ICHAR returns ASCII value
    \item CHAR converts ASCII code to character
    \item Limited to single characters per input
\end{itemize}

\subsection*{Problem 10: Precision Demonstration}
\begin{verbatim}
C     PROBLEM 10 SOLUTION
      PROGRAM PRECISION
      REAL R
      DOUBLE PRECISION D
C     CALCULATIONS
      R = 1.0/3.0
      D = 1.0D0/3.0D0
C     OUTPUT
      WRITE(*,*) 'SINGLE PRECISION:', R
      WRITE(*,*) 'DOUBLE PRECISION:', D
      STOP
      END
\end{verbatim}
\textbf{Output:}
\begin{verbatim}
SINGLE PRECISION:  0.3333333    
DOUBLE PRECISION:  0.3333333333333333
\end{verbatim}
\textbf{Explanation:}
\begin{itemize}
    \item REAL provides ~7 significant digits
    \item DOUBLE PRECISION provides ~15 digits
    \item Use D0 suffix for double-precision literals
\end{itemize}

\subsection*{Challenge Problem: Unit Converter}
\begin{verbatim}
C     CHALLENGE PROBLEM SOLUTION
      PROGRAM UNIT_CONV
      REAL KM, MILES
C     INPUT LOOP
10    WRITE(*,*) 'ENTER KILOMETERS (>=0):'
      READ(*,*) KM
      IF (KM .LT. 0.0) THEN
          WRITE(*,*) 'ERROR: NEGATIVE VALUE!'
          GOTO 10
      ENDIF
C     CONVERSION
      MILES = KM * 0.621371
C     OUTPUT
      WRITE(*,*) KM, 'KM =', MILES, 'MILES'
      STOP
      END
\end{verbatim}
\textbf{Features:}
\begin{itemize}
    \item Input validation with GOTO loop
    \item Real-to-real conversion maintains precision
    \item Clear error messaging
    \item Conversion factor from exact definition
\end{itemize}

\chapter{Conditional Statement in FORTRAN77}

\subsection*{Types of Conditional Statements}
Fortran 77 provides three main conditional constructs:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\textbf{Type} & \textbf{Description} \\ 
\hline
Logical IF & Single-line conditional execution \\
Block IF & Multi-line IF-THEN-ENDIF structure \\
ELSE IF & Multiple alternative conditions \\
Nested IF & IF statements within other IF blocks \\
Arithmetic IF & Three-way branching (legacy) \\
\hline
\end{tabular}
\end{center}

\subsection*{Relational Operators}
\begin{center}
\begin{tabular}{|l|l|l|}
\hline
\textbf{Operator} & \textbf{Meaning} & \textbf{Example} \\ 
\hline
\texttt{.EQ.} & Equal to & \texttt{A .EQ. B} \\
\texttt{.NE.} & Not equal to & \texttt{X .NE. Y} \\
\texttt{.GT.} & Greater than & \texttt{N .GT. 0} \\
\texttt{.GE.} & Greater than or equal & \texttt{AGE .GE. 18} \\
\texttt{.LT.} & Less than & \texttt{TEMP .LT. 32.0} \\
\texttt{.LE.} & Less than or equal & \texttt{COUNT .LE. 10} \\
\hline
\end{tabular}
\end{center}

\subsection*{1. Logical IF (Single-Line)}
Executes one statement if condition is true:
\begin{verbatim}
C     PROGRAM: SINGLE_LINE_IF
C     CHECKS IF NUMBER IS POSITIVE
      PROGRAM LOGIF
      REAL NUM
      WRITE(*,*) 'ENTER A NUMBER:'
      READ(*,*) NUM
      IF (NUM .GT. 0.0) WRITE(*,*) 'POSITIVE NUMBER'
      STOP
      END
\end{verbatim}

\subsection*{2. Block IF Structure}
Executes multiple statements when condition is true:
\begin{verbatim}
C     PROGRAM: TEMPERATURE_CHECK
C     DEMONSTRATES BLOCK IF
      PROGRAM BLKIF
      REAL TEMP
      WRITE(*,*) 'ENTER TEMPERATURE (°C):'
      READ(*,*) TEMP
      
      IF (TEMP .LT. 0.0) THEN
          WRITE(*,*) 'WARNING: BELOW FREEZING!'
          WRITE(*,*) 'TAKE WINTER PRECAUTIONS'
      ENDIF
      
      STOP
      END
\end{verbatim}

\subsection*{3. IF-ELSE Structure}
Handles alternative conditions:
\begin{verbatim}
C     PROGRAM: GRADE_EVALUATOR
C     DEMONSTRATES IF-ELSE
      PROGRAM IFELSE
      INTEGER SCORE
      WRITE(*,*) 'ENTER TEST SCORE (0-100):'
      READ(*,*) SCORE
      
      IF (SCORE .GE. 60) THEN
          WRITE(*,*) 'PASSING GRADE'
      ELSE
          WRITE(*,*) 'FAILING GRADE'
      ENDIF
      
      STOP
      END
\end{verbatim}

\subsection*{4. ELSE IF Ladder}
Handles multiple conditions:
\begin{verbatim}
C     PROGRAM: TAX_BRACKET
C     DEMONSTRATES ELSE IF
      PROGRAM TAXCALC
      REAL INCOME
      WRITE(*,*) 'ENTER ANNUAL INCOME:'
      READ(*,*) INCOME
      
      IF (INCOME .LE. 50000.0) THEN
          WRITE(*,*) 'TAX BRACKET: 10%'
      ELSE IF (INCOME .LE. 100000.0) THEN
          WRITE(*,*) 'TAX BRACKET: 20%'
      ELSE IF (INCOME .LE. 250000.0) THEN
          WRITE(*,*) 'TAX BRACKET: 30%'
      ELSE
          WRITE(*,*) 'TAX BRACKET: 40%'
      ENDIF
      
      STOP
      END
\end{verbatim}

\subsection*{5. Nested IF Statements}
IF blocks within other IF blocks:
\begin{verbatim}
C     PROGRAM: LOGIN_SYSTEM
C     DEMONSTRATES NESTED IF
      PROGRAM LOGIN
      CHARACTER*10 USER
      INTEGER PASS
      LOGICAL ADMIN
      
      WRITE(*,*) 'ENTER USERNAME:'
      READ(*,*) USER
      WRITE(*,*) 'ENTER PASSWORD:'
      READ(*,*) PASS
      
      IF (USER .EQ. 'ADMIN') THEN
          IF (PASS .EQ. 12345) THEN
              ADMIN = .TRUE.
              WRITE(*,*) 'ADMIN ACCESS GRANTED'
          ELSE
              WRITE(*,*) 'INCORRECT PASSWORD'
          ENDIF
      ELSE
          WRITE(*,*) 'GUEST ACCESS ONLY'
      ENDIF
      
      STOP
      END
\end{verbatim}

\subsection*{6. Arithmetic IF (Legacy)}
Three-way branching based on expression sign:
\begin{verbatim}
C     PROGRAM: SIGN_CHECK
C     DEMONSTRATES ARITHMETIC IF (HISTORICAL)
      PROGRAM ARIF
      INTEGER NUM
      WRITE(*,*) 'ENTER AN INTEGER:'
      READ(*,*) NUM
      
      IF (NUM) 10, 20, 30
10    WRITE(*,*) 'NEGATIVE NUMBER'
      GOTO 40
20    WRITE(*,*) 'ZERO'
      GOTO 40
30    WRITE(*,*) 'POSITIVE NUMBER'
40    STOP
      END
\end{verbatim}

\subsection*{Compound Conditions}
Combine conditions with logical operators:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\textbf{Operator} & \textbf{Meaning} \\ 
\hline
\texttt{.AND.} & Both conditions true \\
\texttt{.OR.} & Either condition true \\
\texttt{.NOT.} & Inverts condition \\
\hline
\end{tabular}
\end{center}

\begin{verbatim}
C     PROGRAM: WEATHER_CHECK
C     DEMONSTRATES COMPOUND CONDITIONS
      PROGRAM WEATHER
      REAL TEMP
      LOGICAL RAINING
      
      WRITE(*,*) 'ENTER TEMPERATURE (°C):'
      READ(*,*) TEMP
      WRITE(*,*) 'IS IT RAINING? (.TRUE./.FALSE.):'
      READ(*,*) RAINING
      
      IF (TEMP .GT. 25.0 .AND. .NOT. RAINING) THEN
          WRITE(*,*) 'GOOD DAY FOR BEACH'
      ELSE IF (TEMP .LT. 5.0 .OR. RAINING) THEN
          WRITE(*,*) 'STAY INDOORS'
      ELSE
          WRITE(*,*) 'NORMAL DAY'
      ENDIF
      
      STOP
      END
\end{verbatim}

\subsection*{Common Pitfalls}
\begin{center}
\begin{tabular}{|p{4cm}|p{8cm}|}
\hline
\textbf{Error} & \textbf{Solution} \\ 
\hline
Missing \texttt{ENDIF} & Always match IF with ENDIF \\
\hline
Using \texttt{=} instead of \texttt{.EQ.} & Fortran uses \texttt{.EQ.} for equality \\
\hline
No space around operators & \texttt{.LT.} not \texttt{.LT.} (depends on compiler) \\
\hline
Uninitialized variables & Always initialize variables before use \\
\hline
\end{tabular}
\end{center}

\subsection*{Best Practices}
\begin{itemize}
    \item Use indentation for nested conditionals
    \item Always include \texttt{ELSE} blocks for error handling
    \item Use parentheses for complex logical expressions
    \item Avoid arithmetic IF in new code
    \item Comment complex conditions
    \item Test boundary conditions thoroughly
\end{itemize}

\subsection*{Performance Tips}
\begin{itemize}
    \item Order conditions from most to least likely
    \item Use \texttt{ELSE IF} instead of multiple IFs when mutually exclusive
    \item Avoid deep nesting (max 3-4 levels)
    \item Use logical operators instead of nested IFs when possible
\end{itemize}

\section{Spacing in Nested Conditional Statements}

\subsection*{Fixed-Format Column Rules}
Fortran 77 requires strict column adherence for nested conditionals:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\textbf{Columns} & \textbf{Purpose} \\ 
\hline
1-5 & Optional statement labels \\
6 & Continuation character (if needed) \\
7-72 & Executable code and conditions \\
73+ & Ignored (legacy punch card limit) \\
\hline
\end{tabular}
\end{center}

\subsection*{Indentation Guidelines}
\begin{itemize}
    \item \textbf{Base Level}: Start at column 7 for first IF
    \item \textbf{Nested Level}: Add 3 spaces per nesting level
    \item \textbf{Alignment}: Match THEN/ELSE/ENDIF with their IF level
    \item \textbf{Continuation}: Use column 6 for multi-line conditions
\end{itemize}

\subsection*{Properly Formatted Example}
\begin{verbatim}
C     PROGRAM: NESTED_GRADE_SYSTEM
      PROGRAM NESTED
      INTEGER SCORE
      CHARACTER*1 GRADE
      WRITE(*,*) 'ENTER EXAM SCORE (0-100):'
      READ(*,*) SCORE
      
C     Level 1 IF (column 7)
      IF (SCORE .GE. 90) THEN
C         Level 2 code (column 10)
          GRADE = 'A'
          IF (SCORE .EQ. 100) THEN       ! Level 2 IF (column 10)
C             Level 3 code (column 13)
              WRITE(*,*) 'PERFECT SCORE!'
          END IF                         ! Level 2 END IF
      ELSE IF (SCORE .GE. 80) THEN       ! Level 1 ELSE IF
          GRADE = 'B'
          IF (SCORE .GE. 85) THEN        ! Level 2 IF
              WRITE(*,*) 'NEARLY AN A!'
          END IF
      ELSE
          GRADE = 'F'
      END IF
      
      WRITE(*,*) 'YOUR GRADE: ', GRADE
      STOP
      END
\end{verbatim}

\subsection*{Column Breakdown}
\begin{verbatim}
Columns: 1   5 6 7   72
         |   | | |   |
         v   v v v   v
      IF (X .GT. 0) THEN        <- Level 1 (start at 7)
          IF (Y .LT. 10) THEN   <- Level 2 (+3 spaces)
              Z = X + Y         <- Level 3 (+6 spaces)
          END IF                <- Level 2 alignment
      END IF                    <- Level 1 alignment
\end{verbatim}

\subsection*{Common Spacing Errors}
\begin{center}
\begin{tabular}{|p{4cm}|p{8cm}|}
\hline
\textbf{Error} & \textbf{Solution} \\ 
\hline
Code starts in column 6 & Reserved for continuation markers \\
\hline
Uneven ELSE/END IF alignment & Use same indentation as opening IF \\
\hline
Overlapping columns (past 72) & Break lines with continuation marker \\
\hline
Mixed tabs and spaces & Use spaces only for consistency \\
\hline
\end{tabular}
\end{center}

\subsection*{Best Practices}
\begin{itemize}
    \item Use 3-space indentation per nesting level
    \item Align related keywords vertically:
    \begin{verbatim}
      IF (...) THEN
          ...
      ELSE IF (...) THEN
          ...
      ELSE
          ...
      END IF
    \end{verbatim}
    \item Limit nesting depth to 3-4 levels maximum
    \item Use comments to mark closing END IFs:
    \begin{verbatim}
      END IF  ! CLOSES TEMPERATURE CHECK
    \end{verbatim}
    \item Prefer this:
    \begin{verbatim}
      IF (A .GT. B) THEN
          ...
      END IF
    \end{verbatim}
    Over this:
    \begin{verbatim}
      IF(A.GT.B)THEN
      ...
      ENDIF
    \end{verbatim}
\end{itemize}

\subsection*{Multi-Line Condition Example}
\begin{verbatim}
C     PROGRAM: COMPLEX_CONDITION
      PROGRAM COMPLEX
      REAL X, Y
      LOGICAL FLAG
      WRITE(*,*) 'ENTER X, Y:'
      READ(*,*) X, Y
      
C     Continuation marker (* in column 6)
      IF (X .GT. 100.0 .AND.
     *    Y .LT. 50.0 .OR.
     *    FLAG) THEN
          WRITE(*,*) 'CONDITION MET'
      END IF
      
      STOP
      END
\end{verbatim}

\subsection*{Historical Context}
The strict column rules originate from:
\begin{itemize}
    \item 80-column punch card limitations
    \item Physical card layout requirements
    \item Early compiler design constraints
\end{itemize}

\subsection*{Modern Editor Tips}
\begin{itemize}
    \item Set tab stops at 6, 9, 12, etc.
    \item Enable column guides at 6 and 72
    \item Use syntax highlighting for:
    \begin{itemize}
        \item IF/THEN/ELSE keywords
        \item Continuation markers
        \item Comment lines
    \end{itemize}
    \item Configure auto-indent for nested blocks
\end{itemize}

\subsection*{Troubleshooting Table}
\begin{center}
\begin{tabular}{|p{4cm}|p{8cm}|}
\hline
\textbf{Compiler Error} & \textbf{Spacing Fix} \\ 
\hline
\texttt{Unclassifiable statement} & Check code starts in column 7+ \\
\hline
\texttt{Unterminated IF block} & Align END IF with opening IF \\
\hline
\texttt{Invalid character in column 6} & Remove unintended characters \\
\hline
\texttt{Label field ignored} & Move code from columns 1-5 to 7+ \\
\hline
\end{tabular}
\end{center}


\section{Conditional Statement Examples}

\subsection*{Example 1: Simple Logical IF}
\begin{verbatim}
C     CHECKS IF NUMBER IS POSITIVE
      PROGRAM POSCHK
      REAL NUM
      WRITE(*,*) 'ENTER A NUMBER:'
      READ(*,*) NUM
C     SINGLE-LINE CONDITIONAL
      IF (NUM .GT. 0.0) WRITE(*,*) 'POSITIVE NUMBER'
      STOP
      END
\end{verbatim}
\textbf{Explanation:}
- Uses logical IF for single-condition check
- Executes WRITE only if NUM > 0
- No action for negative/zero values

\subsection*{Example 2: Block IF Structure}
\begin{verbatim}
C     TEMPERATURE STATUS CHECKER
      PROGRAM TEMPSTAT
      REAL TEMP
      WRITE(*,*) 'ENTER TEMPERATURE (°C):'
      READ(*,*) TEMP
      
      IF (TEMP .LT. 0.0) THEN
          WRITE(*,*) 'FREEZING TEMPERATURE!'
      ELSE IF (TEMP .GT. 35.0) THEN
          WRITE(*,*) 'HEAT WARNING!'
      ELSE
          WRITE(*,*) 'NORMAL TEMPERATURE'
      ENDIF
      
      STOP
      END
\end{verbatim}
\textbf{Features:}
- Uses IF-ELSE IF-ELSE structure
- Checks multiple temperature ranges
- Default case for normal temperatures

\subsection*{Example 3: Even/Odd Checker}
\begin{verbatim}
C     DETERMINES IF NUMBER IS EVEN OR ODD
      PROGRAM EVENODD
      INTEGER NUM
      WRITE(*,*) 'ENTER AN INTEGER:'
      READ(*,*) NUM
      
      IF (MOD(NUM,2) .EQ. 0) THEN
          WRITE(*,*) 'EVEN NUMBER'
      ELSE
          WRITE(*,*) 'ODD NUMBER'
      ENDIF
      
      STOP
      END
\end{verbatim}
\textbf{Key Points:}
- Uses MOD intrinsic function
- Compares remainder with .EQ. operator
- Demonstrates simple IF-ELSE structure

\subsection*{Example 4: Grade Calculator}
\begin{verbatim}
C     CONVERTS SCORE TO LETTER GRADE
      PROGRAM GRADE
      INTEGER SCORE
      WRITE(*,*) 'ENTER EXAM SCORE (0-100):'
      READ(*,*) SCORE
      
      IF (SCORE .GE. 90) THEN
          WRITE(*,*) 'GRADE: A'
      ELSE IF (SCORE .GE. 80) THEN
          WRITE(*,*) 'GRADE: B'
      ELSE IF (SCORE .GE. 70) THEN
          WRITE(*,*) 'GRADE: C'
      ELSE IF (SCORE .GE. 60) THEN
          WRITE(*,*) 'GRADE: D'
      ELSE
          WRITE(*,*) 'GRADE: F'
      ENDIF
      
      STOP
      END
\end{verbatim}
\textbf{Notes:}
- Sequential ELSE IF structure
- Conditions checked from highest to lowest
- No overlap between grade ranges

\subsection*{Example 5: Login System}
\begin{verbatim}
C     SIMPLE USER AUTHENTICATION
      PROGRAM LOGIN
      CHARACTER*10 USER
      INTEGER PASS
      WRITE(*,*) 'ENTER USERNAME:'
      READ(*,*) USER
      WRITE(*,*) 'ENTER PASSWORD:'
      READ(*,*) PASS
      
      IF (USER .EQ. 'ADMIN') THEN
          IF (PASS .EQ. 12345) THEN
              WRITE(*,*) 'ACCESS GRANTED'
          ELSE
              WRITE(*,*) 'WRONG PASSWORD'
          ENDIF
      ELSE
          WRITE(*,*) 'INVALID USER'
      ENDIF
      
      STOP
      END
\end{verbatim}
\textbf{Features:}
- Nested IF statements
- Outer check for username
- Inner check for password
- Multiple ELSE conditions

\subsection*{Example 6: Voting Eligibility}
\begin{verbatim}
C     CHECKS VOTING ELIGIBILITY
      PROGRAM VOTE
      INTEGER AGE
      LOGICAL CITIZEN
      WRITE(*,*) 'ENTER AGE:'
      READ(*,*) AGE
      WRITE(*,*) 'CITIZEN? (.TRUE./.FALSE.):'
      READ(*,*) CITIZEN
      
      IF (AGE .GE. 18 .AND. CITIZEN) THEN
          WRITE(*,*) 'ELIGIBLE TO VOTE'
      ELSE
          WRITE(*,*) 'NOT ELIGIBLE'
      ENDIF
      
      STOP
      END
\end{verbatim}
\textbf{Explanation:}
- Uses .AND. logical operator
- Combines multiple conditions
- Requires both conditions to be true

\subsection*{Example 7: Arithmetic IF (Legacy)}
\begin{verbatim}
C     NUMBER SIGN CHECK (HISTORICAL)
      PROGRAM ARIF
      INTEGER NUM
      WRITE(*,*) 'ENTER INTEGER:'
      READ(*,*) NUM
      
      IF (NUM) 10, 20, 30
10    WRITE(*,*) 'NEGATIVE'
      GOTO 40
20    WRITE(*,*) 'ZERO'
      GOTO 40
30    WRITE(*,*) 'POSITIVE'
40    STOP
      END
\end{verbatim}
\textbf{Notes:}
- Uses legacy arithmetic IF
- Branches based on negative/zero/positive
- Requires statement labels
- Not recommended for new code

\subsection*{Example 8: Division Validation}
\begin{verbatim}
C     SAFE DIVISION PROGRAM
      PROGRAM DIVIDE
      REAL A, B, RESULT
      WRITE(*,*) 'ENTER TWO NUMBERS:'
      READ(*,*) A, B
      
      IF (B .EQ. 0.0) THEN
          WRITE(*,*) 'ERROR: DIVISION BY ZERO'
      ELSE
          RESULT = A / B
          WRITE(*,*) 'RESULT:', RESULT
      ENDIF
      
      STOP
      END
\end{verbatim}
\textbf{Key Points:}
- Prevents division by zero
- Uses .EQ. for float comparison
- Error handling before operation

\subsection*{Example 9: Range Checker}
\begin{verbatim}
C     NUMBER RANGE VALIDATION
      PROGRAM RANGE
      INTEGER NUM
      WRITE(*,*) 'ENTER NUMBER (1-100):'
      READ(*,*) NUM
      
      IF (NUM .LT. 1) THEN
          WRITE(*,*) 'TOO SMALL'
      ELSE IF (NUM .GT. 100) THEN
          WRITE(*,*) 'TOO LARGE'
      ELSE
          WRITE(*,*) 'VALID NUMBER'
      ENDIF
      
      STOP
      END
\end{verbatim}
\textbf{Features:}
- Validates input range
- Separate checks for lower/upper bounds
- Else case for valid numbers

\subsection*{Example 10: Simple Calculator}
\begin{verbatim}
C     MENU-DRIVEN CALCULATOR
      PROGRAM CALC
      REAL A, B
      INTEGER CHOICE
      WRITE(*,*) 'ENTER TWO NUMBERS:'
      READ(*,*) A, B
      WRITE(*,*) '1:ADD 2:SUB 3:MUL 4:DIV'
      READ(*,*) CHOICE
      
      IF (CHOICE .EQ. 1) THEN
          WRITE(*,*) 'SUM:', A+B
      ELSE IF (CHOICE .EQ. 2) THEN
          WRITE(*,*) 'DIFF:', A-B
      ELSE IF (CHOICE .EQ. 3) THEN
          WRITE(*,*) 'PRODUCT:', A*B
      ELSE IF (CHOICE .EQ. 4) THEN
          IF (B .NE. 0.0) THEN
              WRITE(*,*) 'QUOTIENT:', A/B
          ELSE
              WRITE(*,*) 'CANNOT DIVIDE BY ZERO'
          ENDIF
      ELSE
          WRITE(*,*) 'INVALID CHOICE'
      ENDIF
      
      STOP
      END
\end{verbatim}
\textbf{Explanation:}
- Nested IF in division case
- Menu-driven interface
- Multiple conditional checks
- Error handling for invalid menu choices

\subsection*{General Notes}
\begin{itemize}
\item All examples use Fortran 77 fixed-format
\item Column 6+ for code, column 1 for comments
\item Use .EQ. instead of == for comparisons
\item ELSE IF must be on same line as ELSE
\item Indentation improves readability
\end{itemize}

\section{Exercises: Conditional Statements}

\subsection*{Problem 1: Basic If-Else}  
Write a program that:  
\begin{itemize}  
\item Reads an integer  
\item Prints "POSITIVE" if >0, "NEGATIVE" if <0, "ZERO" otherwise  
\end{itemize}

\subsection*{Problem 2: Grade Calculator}  
Create a program that:  
\begin{itemize}  
\item Takes a score (0-100) as input  
\item Uses ELSE IF to assign grades:  
  - A (90-100), B (80-89), C (70-79), D (60-69), F (<60)  
\end{itemize}

\subsection*{Problem 3: Voting Eligibility}  
Write a program that:  
\begin{itemize}  
\item Checks if a user can vote  
\item Input: Age and citizenship status (logical)  
\item Output eligibility using .AND. operator  
\end{itemize}

\subsection*{Problem 4: Login System}  
Create a program with:  
\begin{itemize}  
\item Nested IF statements  
\item Checks username (text) and password (number)  
\item Grants access only if both match predefined values  
\end{itemize}

\subsection*{Problem 5: Leap Year Checker}  
Write a program that:  
\begin{itemize}  
\item Determines if a year is a leap year  
\item Conditions: Divisible by 4 but not 100, unless also by 400  
\item Use compound logical operators  
\end{itemize}

\subsection*{Problem 6: Temperature Advisor}  
Create a program that:  
\begin{itemize}  
\item Reads temperature  
\item Advises:  
  - "HOT" (>35°C), "COLD" (<10°C), "MODERATE" otherwise  
\item Use ELSE IF structure  
\end{itemize}

\subsection*{Problem 7: Division Validator}  
Write a program that:  
\begin{itemize}  
\item Takes two numbers  
\item Divides them only if denominator is not equality 0
\item Prints error message for zero denominator  
\end{itemize}

\subsection*{Problem 8: Vowel Checker}  
Create a program that:  
\begin{itemize}  
\item Reads a single character  
\item Uses nested IF to check if it's a vowel (A/E/I/O/U)  
\item Case insensitive (.EQ. with uppercase and lowercase)  
\end{itemize}

\subsection*{Problem 9: Simple Calculator}  
Write a menu-driven program that:  
\begin{itemize}  
\item Takes two numbers and operation choice (1-4)  
\item Performs +, -, *, / based on user selection  
\item Handles invalid menu choices  
\end{itemize}

\subsection*{Problem 10: Number Range Check}  
Create a program that:  
\begin{itemize}  
\item Checks if number is between 1-100  
\item Prints "VALID" or "INVALID"  
\item Adds specific messages for "TOO LOW" (<1) and "TOO HIGH" (>100)  
\end{itemize}

\subsection*{Challenge Problem: ATM Simulator}  
Write a program that:  
\begin{itemize}  
\item Checks PIN (4-digit number)  
\item Checks account balance before withdrawal  
\item Outputs:  
  - "INVALID PIN" if wrong  
  - "INSUFFICIENT FUNDS" if balance < requested amount  
  - "SUCCESS" otherwise  
\end{itemize}

\section{Exercise Answers: Conditional Statements}

\subsection*{Problem 1: Basic If-Else}
\begin{verbatim}
C     DETERMINES NUMBER SIGN
      PROGRAM POSNEG
      INTEGER NUM
      WRITE(*,*) 'ENTER AN INTEGER:'
      READ(*,*) NUM
      
      IF (NUM .GT. 0) THEN
          WRITE(*,*) 'POSITIVE'
      ELSE IF (NUM .LT. 0) THEN
          WRITE(*,*) 'NEGATIVE'
      ELSE
          WRITE(*,*) 'ZERO'
      END IF
      STOP
      END
\end{verbatim}
\textbf{Explanation:}
- Uses IF-ELSE IF-ELSE structure
- Checks >0 first, then <0, default to zero
- .GT. and .LT. relational operators

\subsection*{Problem 2: Grade Calculator}
\begin{verbatim}
C     ASSIGNS LETTER GRADES
      PROGRAM GRADE
      INTEGER SCORE
      WRITE(*,*) 'ENTER SCORE (0-100):'
      READ(*,*) SCORE
      
      IF (SCORE .GE. 90) THEN
          WRITE(*,*) 'GRADE: A'
      ELSE IF (SCORE .GE. 80) THEN
          WRITE(*,*) 'GRADE: B'
      ELSE IF (SCORE .GE. 70) THEN
          WRITE(*,*) 'GRADE: C'
      ELSE IF (SCORE .GE. 60) THEN
          WRITE(*,*) 'GRADE: D'
      ELSE
          WRITE(*,*) 'GRADE: F'
      END IF
      STOP
      END
\end{verbatim}
\textbf{Key Points:}
- ELSE IF ladder structure
- Descending order of conditions
- Inclusive lower bounds

\subsection*{Problem 3: Voting Eligibility}
\begin{verbatim}
C     CHECKS VOTING RIGHTS
      PROGRAM VOTE
      INTEGER AGE
      LOGICAL CITIZEN
      WRITE(*,*) 'ENTER AGE:'
      READ(*,*) AGE
      WRITE(*,*) 'CITIZEN? (.TRUE./.FALSE.):'
      READ(*,*) CITIZEN
      
      IF (AGE .GE. 18 .AND. CITIZEN) THEN
          WRITE(*,*) 'ELIGIBLE TO VOTE'
      ELSE
          WRITE(*,*) 'NOT ELIGIBLE'
      END IF
      STOP
      END
\end{verbatim}
\textbf{Features:}
- Uses .AND. logical operator
- Combines numeric and logical input
- Single condition check

\subsection*{Problem 4: Login System}
\begin{verbatim}
C     SIMPLE AUTHENTICATION
      PROGRAM LOGIN
      CHARACTER*10 USER
      INTEGER PASS
      WRITE(*,*) 'ENTER USERNAME:'
      READ(*,*) USER
      WRITE(*,*) 'ENTER PASSWORD:'
      READ(*,*) PASS
      
      IF (USER .EQ. 'ADMIN') THEN
          IF (PASS .EQ. 1234) THEN
              WRITE(*,*) 'ACCESS GRANTED'
          ELSE
              WRITE(*,*) 'WRONG PASSWORD'
          END IF
      ELSE
          WRITE(*,*) 'INVALID USER'
      END IF
      STOP
      END
\end{verbatim}
\textbf{Explanation:}
- Nested IF structure
- Outer check for username
- Inner check for password
- Character comparison with .EQ.

\subsection*{Problem 5: Leap Year Checker}
\begin{verbatim}
C     DETERMINES LEAP YEARS
      PROGRAM LEAP
      INTEGER YEAR
      LOGICAL COND1, COND2, COND3
      WRITE(*,*) 'ENTER YEAR:'
      READ(*,*) YEAR
      
      COND1 = MOD(YEAR,4) .EQ. 0
      COND2 = MOD(YEAR,100) .NE. 0
      COND3 = MOD(YEAR,400) .EQ. 0
      
      IF ((COND1 .AND. COND2) .OR. COND3) THEN
          WRITE(*,*) 'LEAP YEAR'
      ELSE
          WRITE(*,*) 'NOT A LEAP YEAR'
      END IF
      STOP
      END
\end{verbatim}
\textbf{Logic:}
- Uses MOD for divisibility checks
- Combines conditions with .AND./.OR.
- Follows Gregorian calendar rules

\subsection*{Problem 6: Temperature Advisor}
\begin{verbatim}
C     WEATHER ADVISORY SYSTEM
      PROGRAM TEMPADV
      REAL TEMP
      WRITE(*,*) 'ENTER TEMPERATURE (°C):'
      READ(*,*) TEMP
      
      IF (TEMP .GT. 35.0) THEN
          WRITE(*,*) 'HOT'
      ELSE IF (TEMP .LT. 10.0) THEN
          WRITE(*,*) 'COLD'
      ELSE
          WRITE(*,*) 'MODERATE'
      END IF
      STOP
      END
\end{verbatim}
\textbf{Structure:}
- Three-way ELSE IF
- Floating point comparisons
- Explicit temperature thresholds

\subsection*{Problem 7: Division Validator}
\begin{verbatim}
C     SAFE DIVISION PROGRAM
      PROGRAM DIVIDE
      REAL A, B
      WRITE(*,*) 'ENTER TWO NUMBERS:'
      READ(*,*) A, B
      
      IF (B .EQ. 0.0) THEN
          WRITE(*,*) 'ERROR: DIVISION BY ZERO'
      ELSE
          WRITE(*,*) 'RESULT:', A/B
      END IF
      STOP
      END
\end{verbatim}
\textbf{Safety:}
- Checks denominator before division
- Uses .EQ. for float comparison
- Prevents runtime errors

\subsection*{Problem 8: Vowel Checker}
\begin{verbatim}
C     VOWEL IDENTIFICATION
      PROGRAM VOWEL
      CHARACTER C
      WRITE(*,*) 'ENTER A LETTER:'
      READ(*,*) C
      
      IF (C .EQ. 'A' .OR. C .EQ. 'E' .OR. 
     *    C .EQ. 'I' .OR. C .EQ. 'O' .OR.
     *    C .EQ. 'U' .OR. C .EQ. 'a' .OR.
     *    C .EQ. 'e' .OR. C .EQ. 'i' .OR.
     *    C .EQ. 'o' .OR. C .EQ. 'u') THEN
          WRITE(*,*) 'VOWEL'
      ELSE
          WRITE(*,*) 'NOT A VOWEL'
      END IF
      STOP
      END
\end{verbatim}
\textbf{Features:}
- Multi-line condition with continuation (* in column 6)
- Checks both uppercase and lowercase
- Uses .OR. for multiple possibilities

\subsection*{Problem 9: Simple Calculator}
\begin{verbatim}
C     MENU-DRIVEN CALCULATOR
      PROGRAM CALC
      REAL A, B
      INTEGER CHOICE
      WRITE(*,*) 'ENTER TWO NUMBERS:'
      READ(*,*) A, B
      WRITE(*,*) '1:ADD 2:SUB 3:MUL 4:DIV'
      READ(*,*) CHOICE
      
      IF (CHOICE .EQ. 1) THEN
          WRITE(*,*) 'SUM:', A+B
      ELSE IF (CHOICE .EQ. 2) THEN
          WRITE(*,*) 'DIFFERENCE:', A-B
      ELSE IF (CHOICE .EQ. 3) THEN
          WRITE(*,*) 'PRODUCT:', A*B
      ELSE IF (CHOICE .EQ. 4) THEN
          IF (B .NE. 0.0) THEN
              WRITE(*,*) 'QUOTIENT:', A/B
          ELSE
              WRITE(*,*) 'DIVISION BY ZERO!'
          END IF
      ELSE
          WRITE(*,*) 'INVALID CHOICE'
      END IF
      STOP
      END
\end{verbatim}
\textbf{Structure:}
- Nested IF for division check
- ELSE IF ladder for menu options
- ELSE clause for invalid input

\subsection*{Problem 10: Number Range Check}
\begin{verbatim}
C     RANGE VALIDATION
      PROGRAM RANGE
      INTEGER NUM
      WRITE(*,*) 'ENTER NUMBER (1-100):'
      READ(*,*) NUM
      
      IF (NUM .LT. 1) THEN
          WRITE(*,*) 'TOO LOW'
      ELSE IF (NUM .GT. 100) THEN
          WRITE(*,*) 'TOO HIGH'
      ELSE
          WRITE(*,*) 'VALID'
      END IF
      STOP
      END
\end{verbatim}
\textbf{Logic:}
- Checks lower bound first
- Then upper bound
- Else validates number

\subsection*{Challenge Problem: ATM Simulator}
\begin{verbatim}
C     ATM TRANSACTION SYSTEM
      PROGRAM ATM
      INTEGER PIN, CORRECT_PIN
      REAL BALANCE, AMOUNT
      PARAMETER (CORRECT_PIN = 5678)
      BALANCE = 2500.0
      
      WRITE(*,*) 'ENTER PIN:'
      READ(*,*) PIN
      WRITE(*,*) 'ENTER WITHDRAWAL AMOUNT:'
      READ(*,*) AMOUNT
      
      IF (PIN .NE. CORRECT_PIN) THEN
          WRITE(*,*) 'INVALID PIN'
      ELSE IF (AMOUNT .GT. BALANCE) THEN
          WRITE(*,*) 'INSUFFICIENT FUNDS'
      ELSE
          WRITE(*,*) 'SUCCESS'
      END IF
      STOP
      END
\end{verbatim}
\textbf{Security:}
- PIN validation first
- Balance check second
- PARAMETER for secure PIN storage

\chapter{LOOPS \& LOOPS IN` FORTRAN77}

\section{Loops in Fortran 77}

\subsection*{Types of Loops}
Fortran 77 provides three main looping constructs:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\textbf{Type} & \textbf{Description} \\ 
\hline
DO Loop & Fixed iteration count \\
DO-WHILE & Conditional looping \\
Arithmetic IF (legacy) & GOTO-based iteration \\
\hline
\end{tabular}
\end{center}

\subsection*{1. DO Loop (Fixed Iterations)}
\begin{verbatim}
C     SIMPLE DO LOOP EXAMPLE
      PROGRAM DO_LOOP
      INTEGER I
C     LOOP FROM 1 TO 5 (STEP 1)
      DO 10 I = 1, 5
          WRITE(*,*) 'ITERATION:', I
10    CONTINUE
      STOP
      END
\end{verbatim}
\textbf{Key Features:}
\begin{itemize}
\item \texttt{DO 10 I = 1, 5} - Label 10 marks loop end
\item \texttt{CONTINUE} - Loop termination marker
\item Default step size = 1
\item Loop variable (I) automatically increments
\end{itemize}

\subsection*{DO Loop with Step}
\begin{verbatim}
C     LOOP WITH STEP VALUE
      PROGRAM DO_STEP
      INTEGER N
C     COUNTDOWN FROM 10 TO 0, STEP -2
      DO 20 N = 10, 0, -2
          WRITE(*,*) 'COUNT:', N
20    CONTINUE
      STOP
      END
\end{verbatim}
\textbf{Explanation:}
- Step value (-2) specified after range
- Loop variable decreases by 2 each iteration
- Loop ends when N < 0

\subsection*{2. DO-WHILE Loop (Conditional)}
\begin{verbatim}
C     CONDITIONAL LOOP EXAMPLE
      PROGRAM DOWHILE
      REAL TEMP
      TEMP = 100.0
C     LOOP WHILE TEMPERATURE > 32.0
30    IF (TEMP .GT. 32.0) THEN
          WRITE(*,*) 'CURRENT TEMP:', TEMP
          TEMP = TEMP - 10.0
          GOTO 30
      END IF
      STOP
      END
\end{verbatim}
\textbf{Structure:}
- Label 30 marks loop start
- Condition checked before each iteration
- GOTO creates loopback
- Variable modification inside loop

\subsection*{3. Nested DO Loops}
\begin{verbatim}
C     MULTIPLICATION TABLE GENERATOR
      PROGRAM NESTED
      INTEGER I, J
C     OUTER LOOP (ROWS)
      DO 40 I = 1, 5
C         INNER LOOP (COLUMNS)
          DO 50 J = 1, 5
              WRITE(*,*) I, 'X', J, '=', I*J
50        CONTINUE
40    CONTINUE
      STOP
      END
\end{verbatim}
\textbf{Features:}
- Outer loop (I) controls rows
- Inner loop (J) controls columns
- Unique labels for each loop (40, 50)
- Proper indentation for readability

\subsection*{4. Loop Control Statements}
Fortran 77 has limited control flow:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\textbf{Statement} & \textbf{Purpose} \\ 
\hline
\texttt{GOTO} & Jump to label \\
\texttt{EXIT} & Terminate loop (non-standard) \\
\texttt{CYCLE} & Skip iteration (non-standard) \\
\hline
\end{tabular}
\end{center}

\begin{verbatim}
C     LOOP EXIT EXAMPLE
      PROGRAM LOOPEXIT
      INTEGER COUNT
      COUNT = 1
60    IF (COUNT .LE. 10) THEN
          IF (COUNT .EQ. 5) GOTO 70
          WRITE(*,*) COUNT
          COUNT = COUNT + 1
          GOTO 60
      END IF
70    STOP
      END
\end{verbatim}
\textbf{Explanation:}
- Exits loop when COUNT reaches 5
- Uses GOTO to jump out of loop
- Limited to label-based control

\subsection*{5. Legacy Arithmetic IF Loop}
\begin{verbatim}
C     HISTORICAL APPROACH (NOT RECOMMENDED)
      PROGRAM ARIF
      INTEGER N
      N = 5
80    WRITE(*,*) N
      N = N - 1
      IF (N) 90, 90, 80
90    STOP
      END
\end{verbatim}
\textbf{Behavior:}
- IF (N) 90, 90, 80 branches to:
  - 90 if N < 0
  - 90 if N = 0
  - 80 if N > 0
- Creates countdown from 5 to 0

\subsection*{Loop Variable Rules}
\begin{itemize}
\item Loop variable must be INTEGER
\item Modification inside loop is allowed but discouraged
\item Value persists after loop exit
\item Zero-trip loops possible:
\begin{verbatim}
DO 100 I = 5, 1  ! Never executes
\end{verbatim}
\end{itemize}

\subsection*{Common Loop Patterns}

\subsubsection*{Summation}
\begin{verbatim}
C     SUM FIRST 10 NATURAL NUMBERS
      PROGRAM SUMMATION
      INTEGER I, SUM
      SUM = 0
      DO 110 I = 1, 10
          SUM = SUM + I
110   CONTINUE
      WRITE(*,*) 'TOTAL:', SUM
      STOP
      END
\end{verbatim}

\subsubsection*{Input Validation}
\begin{verbatim}
C     REPEAT UNTIL VALID INPUT
      PROGRAM VALIDATE
      REAL X
120   WRITE(*,*) 'ENTER POSITIVE NUMBER:'
      READ(*,*) X
      IF (X .LE. 0.0) GOTO 120
      WRITE(*,*) 'THANK YOU'
      STOP
      END
\end{verbatim}

\subsection*{Best Practices}
\begin{itemize}
\item Use DO loops for known iterations
\item Prefer DO-WHILE for condition-based loops
\item Avoid modifying loop variables
\item Use unique labels for nested loops
\item Indent loop bodies consistently
\item Comment complex loop logic
\end{itemize}

\subsection*{Common Errors}
\begin{center}
\begin{tabular}{|p{4cm}|p{8cm}|}
\hline
\textbf{Error} & \textbf{Solution} \\ 
\hline
Missing CONTINUE & Ensure every DO has matching label \\
\hline
Infinite loop & Verify exit condition changes \\
\hline
Label mismatch & Check GOTO targets \\
\hline
Real loop variables & Use INTEGER for counters \\
\hline
\end{tabular}
\end{center}

\subsection*{Performance Considerations}
\begin{itemize}
\item Place loop-invariant code outside
\item Minimize I/O inside loops
\item Avoid complex conditions in DO-WHILE
\item Use INTEGER for counters
\item Prefer DO loops over GOTO when possible
\end{itemize}

\section{Loop Examples in Fortran 77}

\subsection*{1. DO Loops (Fixed Iterations)}
\subsubsection*{Example 1: Basic Number Sequence}
\begin{verbatim}
C     PRINT NUMBERS 1 TO 5
      PROGRAM DO1
      INTEGER I
C     START LOOP AT 1, END AT 5, STEP 1
      DO 10 I = 1, 5
          WRITE(*,*) 'NUMBER:', I
10    CONTINUE
      STOP
      END
\end{verbatim}
\textbf{Explanation:}
- Loop variable I starts at 1, increments by 1
- Executes exactly 5 times
- CONTINUE marks loop end (label 10)

\subsubsection*{Example 2: Step Value in Reverse}
\begin{verbatim}
C     COUNTDOWN FROM 10 TO 0
      PROGRAM DO2
      INTEGER COUNT
C     STEP BY -2 (DECREMENT)
      DO 20 COUNT = 10, 0, -2
          WRITE(*,*) 'COUNTDOWN:', COUNT
20    CONTINUE
      STOP
      END
\end{verbatim}
\textbf{Features:}
- Negative step value (-2)
- Loop ends when COUNT < 0
- Output: 10, 8, 6, 4, 2, 0

\subsubsection*{Example 3: Nested Multiplication Table}
\begin{verbatim}
C     5x5 MULTIPLICATION TABLE
      PROGRAM DO3
      INTEGER I, J
C     OUTER LOOP FOR ROWS
      DO 30 I = 1, 5
C         INNER LOOP FOR COLUMNS
          DO 40 J = 1, 5
              WRITE(*,*) I, 'x', J, '=', I*J
40        CONTINUE
30    CONTINUE
      STOP
      END
\end{verbatim}
\textbf{Key Points:}
- Outer loop (I) runs 5 times
- Inner loop (J) completes fully for each I
- Unique labels (30, 40) for each loop

\subsection*{2. DO-WHILE Loops (Conditional)}
\subsubsection*{Example 1: Temperature Monitor}
\begin{verbatim}
C     COOLING SIMULATION
      PROGRAM WHILE1
      REAL TEMP
      TEMP = 100.0
50    IF (TEMP .GT. 32.0) THEN
          WRITE(*,*) 'Current Temp:', TEMP
          TEMP = TEMP - 10.0
          GOTO 50
      END IF
      STOP
      END
\end{verbatim}
\textbf{Explanation:}
- Loop continues while TEMP > 32.0
- GOTO 50 creates loopback
- TEMP decreases by 10 each iteration

\subsubsection*{Example 2: Sum Until Threshold}
\begin{verbatim}
C     SUM NUMBERS UNTIL TOTAL > 100
      PROGRAM WHILE2
      INTEGER NUM, TOTAL
      TOTAL = 0
60    IF (TOTAL .LE. 100) THEN
          WRITE(*,*) 'Enter number:'
          READ(*,*) NUM
          TOTAL = TOTAL + NUM
          GOTO 60
      END IF
      WRITE(*,*) 'Final total:', TOTAL
      STOP
      END
\end{verbatim}
\textbf{Features:}
- Loop until TOTAL exceeds 100
- User input inside loop
- Condition checked before each iteration

\subsubsection*{Example 3: Input Validation}
\begin{verbatim}
C     VALIDATE POSITIVE INPUT
      PROGRAM WHILE3
      REAL X
70    WRITE(*,*) 'Enter positive value:'
      READ(*,*) X
      IF (X .LE. 0.0) THEN
          WRITE(*,*) 'Invalid! Try again'
          GOTO 70
      END IF
      WRITE(*,*) 'Accepted:', X
      STOP
      END
\end{verbatim}
\textbf{Key Points:}
- Forces valid input using GOTO
- Loop continues until X > 0
- No separate loop variable needed

\subsection*{3. Arithmetic IF Loops (Legacy)}
\subsubsection*{Example 1: Simple Countdown}
\begin{verbatim}
C     COUNTDOWN USING ARITHMETIC IF
      PROGRAM ARIF1
      INTEGER N
      N = 5
80    WRITE(*,*) N
      N = N - 1
C     IF(N) neg,zero,pos labels
      IF (N) 90, 90, 80
90    STOP
      END
\end{verbatim}
\textbf{Explanation:}
- IF (N) branches to 90 if N ≤ 0
- Branches to 80 if N > 0
- Output: 5 4 3 2 1 0

\subsubsection*{Example 2: Sum Positive Numbers}
\begin{verbatim}
C     SUM INPUT UNTIL NEGATIVE
      PROGRAM ARIF2
      INTEGER NUM, SUM
      SUM = 0
100   WRITE(*,*) 'Enter number (negative to stop):'
      READ(*,*) NUM
C     BRANCH BASED ON NUM SIGN
      IF (NUM) 110, 120, 120
110   WRITE(*,*) 'Total:', SUM
      STOP
120   SUM = SUM + NUM
      GOTO 100
      END
\end{verbatim}
\textbf{Features:}
- 110: Negative number exit
- 120: Zero/positive accumulation
- Three-way branching

\subsubsection*{Example 3: Password Attempts}
\begin{verbatim}
C     LIMITED PASSWORD ATTEMPTS
      PROGRAM ARIF3
      INTEGER TRIES, PASS
      TRIES = 3
      PASS = 1234
130   WRITE(*,*) 'Enter password (', TRIES, 'left):'
      READ(*,*) INPUT
      IF (INPUT .NE. PASS) THEN
          TRIES = TRIES - 1
          IF (TRIES) 140, 140, 130
      ELSE
          WRITE(*,*) 'Access granted'
          STOP
      END IF
140   WRITE(*,*) 'Account locked'
      STOP
      END
\end{verbatim}
\textbf{Key Points:}
- Gives 3 password attempts
- Uses Arithmetic IF for attempt counting
- Combines modern IF-THEN with legacy branching

\section{Spacing for Loops and Nested Loops}

\subsection*{Fixed-Format Column Rules}
Fortran 77 requires strict adherence to column-based formatting:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\textbf{Columns} & \textbf{Purpose} \\ 
\hline
1-5 & Statement labels (optional) \\
6 & Continuation character \\
7-72 & Executable code \\
73-80 & Ignored (historical) \\
\hline
\end{tabular}
\end{center}

\subsection*{Basic Loop Structure}
\begin{verbatim}
C     BASIC DO LOOP
      PROGRAM LOOP1
      INTEGER I
C     DO statement starts at column 7
      DO 10 I = 1, 5
          WRITE(*,*) I  ! Body indented 3 spaces
10    CONTINUE          ! Label 10 in columns 1-5
      STOP
      END
\end{verbatim}

\subsection*{Nested Loop Spacing}
\begin{verbatim}
C     NESTED LOOPS
      PROGRAM NESTED
      INTEGER I, J
C     Outer loop
      DO 20 I = 1, 3
C         Inner loop (indented 3 spaces)
          DO 30 J = 1, 2
              WRITE(*,*) I, J  ! Double indentation
30        CONTINUE             ! Inner label
20    CONTINUE                 ! Outer label
      STOP
      END
\end{verbatim}

\subsection*{Key Spacing Rules}
\begin{itemize}
\item \textbf{DO Statement}: Start at column 7
\item \textbf{Labels}: Place in columns 1-5
\item \textbf{Body}: Indent 3-6 spaces per nesting level
\item \textbf{CONTINUE}: Align with corresponding DO
\end{itemize}

\subsection*{Proper Column Layout}
\begin{verbatim}
Columns: 1   5 6 7   72
         |   | | |   |
         v   v v v   v
      DO 40 I = 1, 3     <- Outer loop (col 7)
          DO 50 J = 1, 2 <- Inner loop (+3 spaces)
              ...        <- Body (+6 spaces)
50        CONTINUE       <- Inner label (col 1-5)
40    CONTINUE           <- Outer label
\end{verbatim}

\subsection*{Common Mistakes}
\begin{center}
\begin{tabular}{|p{5cm}|p{9cm}|}
\hline
\textbf{Error} & \textbf{Solution} \\ 
\hline
Code starts in column 6 & Shift to column 7+ \\
\hline
Missing CONTINUE label & Ensure every DO has matching label \\
\hline
Overlapping labels & Use unique numbers (10, 20, 30, etc.) \\
\hline
Body not indented & Add 3-6 spaces per nesting level \\
\hline
\end{tabular}
\end{center}

\subsection*{Best Practices}
\begin{itemize}
\item \textbf{Indentation}: Use 3 spaces per nesting level
\item \textbf{Labels}: Increment by 10s (10, 20, 30) for flexibility
\item \textbf{Comments}: Describe loop purpose
\item \textbf{Deep Nesting}: Avoid beyond 3 levels
\item \textbf{Variable Names}: Use meaningful names (ROW/COL vs I/J)
\end{itemize}

\subsection*{Advanced Example: Triple Nested Loop}
\begin{verbatim}
C     3D MATRIX INITIALIZATION
      PROGRAM TRIPLE
      INTEGER X, Y, Z
C     Outer loop
      DO 100 X = 1, 2
C         Middle loop
          DO 200 Y = 1, 3
C             Inner loop
              DO 300 Z = 1, 2
                  WRITE(*,*) X, Y, Z
300           CONTINUE
200       CONTINUE
100   CONTINUE
      STOP
      END
\end{verbatim}

\subsection*{Legacy Approach (Arithmetic IF)}
\begin{verbatim}
C     NOT RECOMMENDED - HISTORICAL USE
      PROGRAM LEGACY
      INTEGER K
      K = 1
400   WRITE(*,*) K
      K = K + 1
      IF (K - 5) 400, 400, 500
500   STOP
      END
\end{verbatim}

\subsection*{Performance Tips}
\begin{itemize}
\item Place WRITE/READ outside loops when possible
\item Prefer DO loops over GOTO for readability
\item Initialize variables before loops
\item Avoid modifying loop counters
\end{itemize}
\section{Exercises: Loops in Fortran 77}

\subsection*{Problem 1: Basic DO Loop}  
Write a program that:  
\begin{itemize}  
\item Uses a DO loop to print numbers 1 through 10  
\item Follows fixed-format column rules  
\item Uses a CONTINUE statement  
\end{itemize}

\subsection*{Problem 2: Step Value Practice}  
Create a program that:  
\begin{itemize}  
\item Prints even numbers between 2 and 20  
\item Uses a DO loop with step value 2  
\item Labels loop termination properly  
\end{itemize}

\subsection*{Problem 3: Nested Loop Grid}  
Write a program that:  
\begin{itemize}  
\item Uses nested DO loops to print all (i,j) pairs for a 3x3 grid  
\item Outer loop for i-values (1-3)  
\item Inner loop for j-values (1-3)  
\end{itemize}

\subsection*{Problem 4: Conditional Summation}  
Create a program that:  
\begin{itemize}  
\item Uses a DO-WHILE structure (IF-GOTO)  
\item Accumulates numbers until total exceeds 100  
\item Shows intermediate sums  
\end{itemize}

\subsection*{Problem 5: Input Validation}  
Write a program that:  
\begin{itemize}  
\item Repeatedly asks for positive number input  
\item Uses a DO-WHILE loop with .LE. operator  
\item Exits only when valid input received  
\end{itemize}

\subsection*{Problem 6: Pattern Printing}  
Create a program that:  
\begin{itemize}  
\item Uses nested loops to print:  
\begin{verbatim}
*
**
***
\end{verbatim}  
\item Each level adds one more asterisk  
\end{itemize}

\subsection*{Problem 7: Factorial Calculator}  
Write a program that:  
\begin{itemize}  
\item Calculates factorial of user-input number  
\item Uses a DO loop for multiplication  
\item Handles 0! = 1 case  
\end{itemize}

\subsection*{Problem 8: Early Exit Loop}  
Create a program that:  
\begin{itemize}  
\item Reads numbers until negative entered  
\item Uses GOTO to exit loop early  
\item Accumulates positive numbers  
\end{itemize}

\subsection*{Problem 9: Legacy Countdown}  
Write a program that:  
\begin{itemize}  
\item Uses arithmetic IF loop structure  
\item Counts down from 5 to 1  
\item Prints "LIFTOFF!" at end  
\end{itemize}

\subsection*{Problem 10: Login System}  
Create a program that:  
\begin{itemize}  
\item Gives 3 password attempts  
\item Uses loop with attempt counter  
\item Shows remaining attempts  
\item Uses fixed-format spacing  
\end{itemize}

\subsection*{Challenge Problem: Prime Checker}  
Write a program that:  
\begin{itemize}  
\item Checks if input number is prime  
\item Uses nested loops and MOD function  
\item Optimizes loop range for efficiency  
\end{itemize}

\section{Exercise Answers: Loops in Fortran 77}

\subsection*{Problem 1: Basic DO Loop}
\begin{verbatim}
C     PRINTS NUMBERS 1 TO 10
      PROGRAM DO_LOOP
      INTEGER I
C     LOOP FROM 1 TO 10
      DO 10 I = 1, 10
          WRITE(*,*) I
10    CONTINUE
      STOP
      END
\end{verbatim}
\textbf{Explanation:}
- Loop variable \texttt{I} runs from 1 to 10
- \texttt{CONTINUE} at label 10 marks loop end
- Implicit increment of 1

\subsection*{Problem 2: Step Value Practice}
\begin{verbatim}
C     PRINTS EVEN NUMBERS 2-20
      PROGRAM EVENS
      INTEGER N
C     STEP BY 2
      DO 20 N = 2, 20, 2
          WRITE(*,*) N
20    CONTINUE
      STOP
      END
\end{verbatim}
\textbf{Features:}
- Step value 2 specified
- Loop ends at 20 (inclusive)
- Output: 2, 4, 6,..., 20

\subsection*{Problem 3: Nested Loop Grid}
\begin{verbatim}
C     PRINTS 3x3 GRID COORDINATES
      PROGRAM GRID
      INTEGER I, J
C     OUTER LOOP (ROWS)
      DO 30 I = 1, 3
C         INNER LOOP (COLUMNS)
          DO 40 J = 1, 3
              WRITE(*,*) '(', I, ',', J, ')'
40        CONTINUE
30    CONTINUE
      STOP
      END
\end{verbatim}
\textbf{Output:}
\begin{verbatim}
(1,1)
(1,2)
...
(3,3)
\end{verbatim}

\subsection*{Problem 4: Conditional Summation}
\begin{verbatim}
C     SUMS NUMBERS UNTIL >100
      PROGRAM SUM100
      INTEGER NUM, TOTAL
      TOTAL = 0
50    IF (TOTAL .LE. 100) THEN
          WRITE(*,*) 'Current total:', TOTAL
          WRITE(*,*) 'Enter number:'
          READ(*,*) NUM
          TOTAL = TOTAL + NUM
          GOTO 50
      END IF
      WRITE(*,*) 'Final total:', TOTAL
      STOP
      END
\end{verbatim}
\textbf{Logic:}
- Loop continues while total ≤ 100
- User input inside loop
- \texttt{GOTO 50} creates repetition

\subsection*{Problem 5: Input Validation}
\begin{verbatim}
C     ENSURES POSITIVE INPUT
      PROGRAM VALIDATE
      REAL X
60    WRITE(*,*) 'Enter positive number:'
      READ(*,*) X
      IF (X .LE. 0.0) THEN
          WRITE(*,*) 'Invalid input!'
          GOTO 60
      END IF
      WRITE(*,*) 'Accepted:', X
      STOP
      END
\end{verbatim}
\textbf{Key Points:}
- Forces valid input using \texttt{GOTO}
- Loop continues until X > 0
- No explicit loop counter needed

\subsection*{Problem 6: Pattern Printing}
\begin{verbatim}
C     PRINTS RIGHT TRIANGLE PATTERN
      PROGRAM PATTERN
      INTEGER I, J
      DO 70 I = 1, 3
          DO 80 J = 1, I
              WRITE(*,*) '*'
80        CONTINUE
          WRITE(*,*) ' '  ! Newline
70    CONTINUE
      STOP
      END
\end{verbatim}
\textbf{Output:}
\begin{verbatim}
*
**
***
\end{verbatim}

\subsection*{Problem 7: Factorial Calculator}
\begin{verbatim}
C     CALCULATES N!
      PROGRAM FACT
      INTEGER N, I, FACTORIAL
      FACTORIAL = 1
      WRITE(*,*) 'Enter number:'
      READ(*,*) N
C     HANDLE 0! = 1 CASE
      IF (N .EQ. 0) GOTO 90
      DO 100 I = 1, N
          FACTORIAL = FACTORIAL * I
100   CONTINUE
90    WRITE(*,*) N, '! =', FACTORIAL
      STOP
      END
\end{verbatim}
\textbf{Note:}
- Special case for 0! handled
- Loop multiplies sequentially

\subsection*{Problem 8: Early Exit Loop}
\begin{verbatim}
C     SUMS POSITIVE NUMBERS
      PROGRAM SUM_POS
      INTEGER NUM, TOTAL
      TOTAL = 0
110   WRITE(*,*) 'Enter number (negative to stop):'
      READ(*,*) NUM
      IF (NUM .LT. 0) GOTO 120
      TOTAL = TOTAL + NUM
      GOTO 110
120   WRITE(*,*) 'Total:', TOTAL
      STOP
      END
\end{verbatim}
\textbf{Explanation:}
- \texttt{GOTO 120} exits on negative input
- Accumulates positive numbers
- Infinite loop until exit condition

\subsection*{Problem 9: Legacy Countdown}
\begin{verbatim}
C     COUNTDOWN USING ARITHMETIC IF
      PROGRAM LIFTOFF
      INTEGER K
      K = 5
130   WRITE(*,*) K
      K = K - 1
      IF (K) 140, 140, 130
140   WRITE(*,*) 'LIFTOFF!'
      STOP
      END
\end{verbatim}
\textbf{Output:}
\begin{verbatim}
5
4
3
2
1
LIFTOFF!
\end{verbatim}

\subsection*{Problem 10: Login System}
\begin{verbatim}
C     PASSWORD ATTEMPT SYSTEM
      PROGRAM LOGIN
      INTEGER TRIES, PASS
      TRIES = 3
      PASS = 1234
150   WRITE(*,*) 'Attempts left:', TRIES
      WRITE(*,*) 'Enter password:'
      READ(*,*) INPUT
      IF (INPUT .EQ. PASS) THEN
          WRITE(*,*) 'Access granted!'
          STOP
      END IF
      TRIES = TRIES - 1
      IF (TRIES .GT. 0) GOTO 150
      WRITE(*,*) 'Account locked!'
      STOP
      END
\end{verbatim}
\textbf{Features:}
- 3 attempt counter
- \texttt{GOTO} for loop control
- Checks password match

\subsection{Challenge Problem: Prime Checker}
\begin{verbatim}
C     CHECKS PRIME NUMBERS
      PROGRAM PRIME
      INTEGER N, I
      LOGICAL ISPRIME
      ISPRIME = .TRUE.
      WRITE(*,*) 'Enter number:'
      READ(*,*) N
C     CHECK DIVISORS UP TO SQRT(N)
      DO 160 I = 2, INT(SQRT(REAL(N)))
          IF (MOD(N, I) .EQ. 0) THEN
              ISPRIME = .FALSE.
              EXIT
          END IF
160   CONTINUE
      IF (ISPRIME) THEN
          WRITE(*,*) N, 'is prime'
      ELSE
          WRITE(*,*) N, 'is not prime'
      END IF
      STOP
      END
\end{verbatim}
\textbf{Optimization:}
- Loops only up to square root of n
- Uses \texttt{EXIT} for early termination
- \texttt{MOD} checks divisibility

\chapter{Arrays in Fortran 77}

\section*{Introduction to Arrays}
Arrays allow storage and manipulation of multiple values of the same type. They are essential for handling datasets, matrices, and structured data. Fortran 77 supports static arrays with fixed sizes determined at compile time.

\section*{Declaring Arrays}
\subsection*{One-Dimensional Arrays}
\begin{verbatim}
C     DECLARING 1D ARRAYS
      PROGRAM ARRAY_DECLARE
      INTEGER NUMBERS(5)      ! 5-element integer array
      REAL    TEMPS(0:10)     ! 11 elements (0-10)
      LOGICAL FLAGS(3)        ! 3-element logical array
      CHARACTER*10 NAMES(4)   ! 4 strings of 10 chars each
      
      NUMBERS(1) = 10        ! Access first element
      TEMPS(0) = 23.5        ! Index starts at 0
      STOP
      END
\end{verbatim}

\subsection*{Multi-Dimensional Arrays}
\begin{verbatim}
C     2D ARRAY DECLARATION
      PROGRAM MATRIX_DECLARE
      REAL GRID(3,3)         ! 3x3 matrix
      INTEGER CUBE(2,2,2)    ! 2x2x2 3D array
      
      GRID(2,1) = 4.7       ! Row 2, Column 1
      STOP
      END
\end{verbatim}

\section*{Initializing Arrays}
\subsection*{DATA Statement}
\begin{verbatim}
C     COMPILE-TIME INITIALIZATION
      PROGRAM DATA_INIT
      INTEGER MARKS(5)
      DATA MARKS /85, 90, 78, 92, 88/
      
      REAL MATRIX(2,2)
      DATA MATRIX /1.0, 2.0, 3.0, 4.0/ ! Column-wise filling
      STOP
      END
\end{verbatim}

\subsection*{Runtime Initialization}
\begin{verbatim}
C     LOOP INITIALIZATION
      PROGRAM LOOP_INIT
      REAL SQUARES(10)
      INTEGER I
      
      DO 10 I = 1, 10
          SQUARES(I) = I**2
10    CONTINUE
      STOP
      END
\end{verbatim}

\section*{Accessing Array Elements}
\begin{verbatim}
C     MATRIX SUMMATION EXAMPLE
      PROGRAM MAT_SUM
      REAL A(3,3), TOTAL
      INTEGER I, J
      
C     Initialize matrix
      DO 20 I = 1, 3
          DO 30 J = 1, 3
              A(I,J) = I + J
30        CONTINUE
20    CONTINUE

C     Calculate sum
      TOTAL = 0.0
      DO 40 I = 1, 3
          DO 50 J = 1, 3
              TOTAL = TOTAL + A(I,J)
50        CONTINUE
40    CONTINUE
      WRITE(*,*) 'Total sum:', TOTAL
      STOP
      END
\end{verbatim}

\section{Passing Arrays to Subprograms}
\subsection{Main Program}
\begin{verbatim}
      PROGRAM MAIN
      INTEGER ARR(5)
      DATA ARR /1,2,3,4,5/
      CALL PRINT_ARRAY(ARR, 5)
      STOP
      END
\end{verbatim}

\subsection{Subroutine}
\begin{verbatim}
C     ADJUSTABLE ARRAY IN SUBROUTINE
      SUBROUTINE PRINT_ARRAY(A, N)
      INTEGER N, A(N)
      INTEGER I
      
      DO 60 I = 1, N
          WRITE(*,*) 'Element', I, '=', A(I)
60    CONTINUE
      RETURN
      END
\end{verbatim}

\section{Array Operations}
\subsection*{Element-wise Operations}
\begin{verbatim}
C     VECTOR ADDITION
      PROGRAM VEC_ADD
      REAL V1(5), V2(5), RESULT(5)
      INTEGER I
      
C     Initialize vectors
      DO 70 I = 1, 5
          V1(I) = I
          V2(I) = I*2
70    CONTINUE

C     Perform addition
      DO 80 I = 1, 5
          RESULT(I) = V1(I) + V2(I)
80    CONTINUE
      STOP
      END
\end{verbatim}

\section{Common Pitfalls}
\begin{itemize}
\item \textbf{Out-of-Bounds Access:}
\begin{verbatim}
INTEGER ARR(5)
ARR(6) = 10  ! Undefined behavior
\end{verbatim}

\item \textbf{Column-Major Order:}
\begin{verbatim}
REAL MAT(100,100)
! More efficient:
DO 100 J = 1, 100   ! Columns outer loop
    DO 200 I = 1, 100
        MAT(I,J) = ...
200 CONTINUE
100 CONTINUE
\end{verbatim}

\item \textbf{Size Mismatch:}
\begin{verbatim}
CALL SUB(ARR(5)) when SUB expects ARR(10)
\end{verbatim}
\end{itemize}

\section{Best Practices}
\begin{itemize}
\item Use PARAMETER for array sizes:
\begin{verbatim}
INTEGER, PARAMETER :: SIZE = 100
REAL DATA(SIZE)
\end{verbatim}

\item Initialize arrays explicitly
\item Comment array dimensions and purposes
\item Prefer column-wise iteration for matrices
\end{itemize}

\section*{Advanced Example: Matrix Multiplication}
\begin{verbatim}
C     MATRIX MULTIPLICATION
      PROGRAM MAT_MUL
      REAL A(2,2), B(2,2), C(2,2)
      INTEGER I, J, K
      
C     Initialize matrices
      DATA A /1.0, 2.0, 3.0, 4.0/
      DATA B /5.0, 6.0, 7.0, 8.0/

C     Perform multiplication
      DO 300 I = 1, 2
          DO 400 J = 1, 2
              C(I,J) = 0.0
              DO 500 K = 1, 2
                  C(I,J) = C(I,J) + A(I,K)*B(K,J)
500           CONTINUE
400       CONTINUE
300   CONTINUE

C     Print result
      WRITE(*,*) 'Product matrix:'
      DO 600 I = 1, 2
          WRITE(*,*) C(I,1), C(I,2)
600   CONTINUE
      STOP
      END
\end{verbatim}



\end{document}
