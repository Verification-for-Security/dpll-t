# VoS Assignment-2

## Task

Implement an **SMT solver** over the theory of Linear Rational Arithmetic (LRA)

- 使用 DPLL 算法实现 SAT 求解器（主要内容）

  1. Conversion from proposition into Conjunct Normal Form (**CNF**)
  2. **Tseitins Transformation** to create an equisatisfiable CNF
  3. The **DPLL algorithm**, including BCP and PLE

- 当您的 SAT 求解器完全运行后，您可以通过加入理论求解器将其扩展为 SMT 求解器

  > 在这种情况下，可以使用单纯形法对 LRA 进行求解。In this case, one over LRA using the Simplex method.

  1. Conversion from LRA to a format supported by the **theory solver (Simplex)**
  2. The **DPLL(T) algorithm**

## 结构

- src

  - 【AST】

    - LRA.hs（没有需要完成的）
  
      > 自定义**数据类型 `LRA`**，它用于表示线性实数算术表达式中的**不等式关系**。`LRA` 的构造函数有两种形式，分别表示小于等于（Greater than or Equal to，:<=:）和大于等于（Less than or Equal to，:>=:）的不等式关系。
      >
      > 定义了一个名为 **`Expr` 的自定义数据类型**，用于表示**数学表达式**，其中包括变量、常数、一元负号、加法和乘法操作。该数据类型的目的是用于构建和处理数学表达式的抽象语法树。接下来的 `deriving` 语句表明 `Expr` 类型可以自动派生（derive）一些类型类的实例，包括 `Eq`、`Ord`、`Functor`、`Foldable` 和 `Traversable`。这些实例允许 `Expr` 类型具有相等性比较、大小比较以及可遍历等功能。
  
    - Prop.hs
  
       > 1. 定义prop：Propositional logic format
       >  
       > 2. 定义了一个函数 `assocr`，用于对命题逻辑表达式进行右结合（right-associative）的转换。这个函数的主要目的是提高打印表达式时的可读性，通过减少括号的使用来使表达式更易于理解。这在命题逻辑的上下文中很有用，因为逻辑操作符 `:&:` 和 `:|:` 是可交换的（commutative），因此可以通过右结合来减少括号的数量。
       3. 自定义的运算符 `(-->)` 用于表示命题逻辑中的蕴含（Implication）
       3. 自定义的运算符 `<->`，用于表示命题逻辑中的双蕴含（Bi-Implication）
  
  - 【CNF】
  
    - DPLL.hs
  
      1. Unit Resolution **UR 函数**
  
      2. Pure Literral Elimination **PLE 函数**
  
      3. Boolean Constraint Propagation **BCP 函数**
  
      4. try ???
  
         > 试图通过解决给定literal来解决约束条件。以这种方式选择约束条件可能会导致递归 dpll 调用失败，这也是此函数称为 "try "的原因。
         >  您可能希望将此函数作为 分支 "的辅助函数。
  
      5. 深度查询函数
  
      6. **DPLL函数**
  
      7. Check函数
  
    - Transfrom.hs
  
      1. distribute函数（分配律函数）：the distribution of disjunction over conjunction
      2. cnf函数（转换成CNF的函数）：Converts a proposition into a rigid CNF
  
    - Tseutin.hs
  
      1. Tseitins transformation实现函数
  
    - Types.hs
  
      > 类型定义文件：
      >
      > module CNF.Types
      >   ( CNF
      >   , Or
      >   , Lit (..)
      >   , negate
      >   ) where
      >
      > - Lit的意思是Literal，In a CNF, we either have a literal or a negation of it.
  
  - 【Parser】
  
    - LRA.hs
  
      > 构建了一个解析器，可以将文本表示的LRA表达式解析为内部数据结构，并将其表示为命题逻辑表达式。
      >
      > TODO:代码中的某个地方正在解析常数（`Constant`），但实际上应该解析有理数（`Rational`）而不是自然数（`natural`）
  
    - Lexeme.hs
  
      > 提供了一些通用的词法分析函数，用于解析文本中的标识符、操作符、括号和自然数，并确保在解析时跳过不必要的空白字符
  
    - Prop.hs
  
      > 提供了一组函数和规则，用于解析命题逻辑表达式。它构建了一个解析器，可以将文本表示的命题逻辑表达式解析为内部数据结构，其中包括否定、合取、析取、蕴含和双蕴含等命题逻辑运算符
  
  - 【Theory】
  
    - Class.hs
  
      > 定义了一个通用的 `Theory` 类型类，该类型类表示理论，并要求每个实现它的具体类型提供一个关联类型 `Model a` 和一个解决约束的函数 `solve`。
  
    - Simplex.hs
  
      > 实现了 Simplex 方法在特定类型（Simplex.PolyConstraint）上的线性规划理论实例。它定义了如何解决 Simplex.PolyConstraint 约束
  
      1. **`simplex :: LRA ID -> Maybe Simplex.PolyConstraint`**：
         - 这个函数接受一个 `LRA ID` 类型的表达式，尝试将其转换为 Simplex 线性规划库所需的刚性线性形式。刚性线性形式通常表示为 `a1 * x1 + a2 * x2 + ... + an * xn = b` 的形式，其中 `ai` 是系数，`xi` 是变量，`b` 是常数。
         - 如果输入的表达式已经处于这种形式，那么函数可能会尝试将其转换为 `Simplex.PolyConstraint` 类型并返回结果。否则，函数返回 `Nothing` 表示无法进行转换。
  
      2. **`linear :: Expr ID -> Maybe Simplex.VarConstMap`**：
         - 这个函数接受一个 `Expr ID` 类型的表达式，尝试将其转换为 Simplex 线性规划库所需的线性形式。线性形式通常表示为 `a1 * x1 + a2 * x2 + ... + an * xn` 的形式，其中 `ai` 是系数，`xi` 是变量。
         - 如果输入的表达式已经处于这种形式，那么函数可能会尝试将其转换为 `Simplex.VarConstMap` 类型并返回结果。否则，函数返回 `Nothing` 表示无法进行转换。
  
      3. **`variable :: Expr ID -> Maybe Integer`**：
         - 这个函数接受一个 `Expr ID` 类型的表达式，尝试提取其中的变量，并将其转换为整数。如果表达式代表一个变量，那么函数返回变量的整数标识。否则，函数返回 `Nothing` 表示无法提取变量。
  
      4. **`constant :: Expr ID -> Maybe Rational`**：
         - 这个函数接受一个 `Expr ID` 类型的表达式，尝试提取其中的常数，并将其转换为有理数。如果表达式代表一个常数，那么函数返回常数的有理数表示。否则，函数返回 `Nothing` 表示无法提取常数。
  
  - CNF.HS
  
    ```haskell
    module CNF
      ( CNF
      , Or
      , Lit (..)
      , Solution
    
      , negate
      , cnf
      , equisat
      , satisfiable
      ) where
    
    import Prelude hiding (negate)
    
    import CNF.DPLL (Solution, satisfiable)
    import CNF.Transform (cnf)
    import CNF.Tseitin (equisat)
    import CNF.Types (CNF, Or, Lit (..), negate)
    ```
  
  - Parser.hs
  
    ```haskell
    module Parser
      ( bool
      , lra
      ) where
    
    import AST.Prop
    import AST.LRA
    
    import Parser.LRA
    import Parser.Prop
    import Parser.Lexeme
    
    import Data.Text (Text)
    
    import Text.Parsec hiding (parse)
    
    program :: Stream s m Char => ParsecT s u m (Prop a) -> ParsecT s u m (Prop a)
    program atom = do
      spaces
      p <- prop atom
      eof
      return p
    
    bool :: Stream s m Char => ParsecT s u m (Prop Text)
    bool = program $ fmap Lit identifier
    
    lra :: Stream s m Char => ParsecT s u m (Prop (LRA Text))
    lra = program constraint
    ```
  
  - Rename.hs
  
    ```haskell
    module Rename
      ( ID (..)
      , Renames
    
      , lookupName
      , withIDs
      , rename
      ) where
    
    import Data.Map (Map)
    import qualified Data.Map as Map
    
    import Control.Monad.State
    
    -- | An identifier for variables for which we can
    -- easily generate fresh variables.
    newtype ID = ID Integer
      deriving (Eq, Ord, Num)
    
    instance Show ID where
      show (ID idx) = 'x' : show idx
    
    -- | A mapping to go from identifiers back to their
    -- original representation.
    newtype Renames a = Renames (Map ID a)
    
    lookupName :: Renames a -> ID -> Maybe a
    lookupName (Renames m) ident = Map.lookup ident m
    
    -- | This is scaffolding code not needed for your implementation. It adds IDs
    -- for literals. Literals with the same name get the same IDs.
    addID :: (MonadState (ID, Map a ID) m, Ord a) => a -> m ID
    addID e = do
      (ident, m) <- get
      case Map.lookup e m of
        Just ident' -> return ident'
        Nothing -> do 
          let ident' = ident + 1
          let m' = Map.insert e ident m
          put (ident', m')
          return ident
    
    withIDs :: (MonadState (ID, Map a ID) m, Ord a, Traversable f) => f a -> m (f ID)
    withIDs = mapM addID
    
    rename :: (Traversable f, Ord a) => f a -> (ID, f ID, Renames a)
    rename f = (next, f', Renames $ invert renames)
      where
        (f', (next, renames)) = runState (withIDs f) (0, Map.empty)
    
    invert :: Ord b => Map a b -> Map b a
    invert = Map.foldrWithKey (flip Map.insert) Map.empty
    ```
  
    
  
  - SMT.hs
  
    ```haskell
    module SMT
      ( smt
      ) where
    
    import Prelude hiding (negate)
    
    import Rename
    import CNF
    import AST.Prop (Prop)
    import Theory.Class
    
    import Control.Applicative
    import Data.Maybe (mapMaybe)
    
    -- | Rename a solution so it becomes a set of constraints for a Theory.
    --
    -- Notice that Solution and Constraints are essentially the same type
    -- under a different name.
    asConstraints :: Renames a -> Solution ID -> Constraints a
    asConstraints = undefined
    
    -- | The DPLL(T) algorithm.
    --
    -- Extends the normal DPLL algorithm to work over theorems.
    -- This implementation solves only over a single theorem at a time.
    --
    -- The algorithm works as follows:
    -- 1.  Find a solution over a CNF ID, negligent of what the IDs represent.
    -- 2.  Get theory constraints by replacing the IDs of this SAT solution to
    --     their corresponding theory symbols.
    -- 3.  Try to solve this new set of constraints.
    -- a.  On succes. We return the Model
    -- b.  On failure. We strenghthen our CNF with an element wise negation of the
    --     original solution over IDs, then recursively call dpllT.
    --
    -- Notice how in step 3b we essentially tell the SAT solver to not pick this
    -- specific solution via the additional constraints. Otherwise the SAT solver 
    -- would infinitely pick the same solution! 
    --
    -- Hint: Check the 'Theory' typeclass.
    dpllT :: Theory a => CNF ID -> Renames a -> Maybe (Model a)
    dpllT = undefined
    
    -- | Run the SMT solver on a proposition over some solvable
    -- theory.
    smt :: (Theory a, Ord a) => Prop a -> Maybe (Model a)
    smt = uncurry dpllT . equisat
    ```
  
    