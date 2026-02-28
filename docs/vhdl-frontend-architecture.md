# VHDL Frontend Architecture

A synthesizable-VHDL frontend for skalp that parses VHDL-2008/2019 and lowers
it to HIR, reusing the entire backend pipeline (MIR, SIR, LIR, codegen, sim,
formal).

## Motivation

skalp's backend is language-agnostic after HIR. A VHDL frontend gives
existing VHDL users access to:

- **Rust-native testbench** (`skalp_testing::Testbench`, async, cycle-accurate)
- **Behavioral simulation** (compiled C++ via SIR, no external simulator)
- **Formal verification** (built-in model checking)
- **SystemVerilog codegen** (for FPGA/ASIC flows that require SV)
- **Safety analysis** (ISO 26262 FMEDA, TMR insertion)

without rewriting their RTL. VHDL is the natural first target because its
type system (records, enumerations, constrained subtypes, generics) maps
cleanly to skalp's HIR.

---

## Positioning: Why This Matters

No open-source tool — and arguably no single commercial tool — offers
all of these capabilities in one integrated package. Here is where skalp
with a VHDL frontend sits relative to the current landscape:

| Capability | Vivado/Quartus | ModelSim/Questa | GHDL | NVC | Sigasi | **skalp** |
|---|---|---|---|---|---|---|
| VHDL-2008 support | Partial | Good | Excellent | Good | Parse only | **Full (synth)** |
| VHDL-2019 interfaces/views | No | Partial | Partial | No | No | **Full** |
| Simulation | Vendor sim | $10-50k/seat/yr | Free (interpreted) | Free (interpreted) | No | **Free (compiled C++)** |
| Formal verification | Separate tool ($$$) | Separate (Questa Formal) | No | No | No | **Built in** |
| CDC analysis | Separate tool ($$$) | No | No | No | No | **Built in (compile-time)** |
| Safety analysis (ISO 26262) | No | No | No | No | No | **Built in** |
| Rust-native testbench | No | No | No | No | No | **Yes (`cargo test`)** |
| Language server (IDE) | Basic | No | Basic (ghdl-ls) | No | Yes ($1-5k/yr) | **Yes (free)** |
| Conditional breakpoints | GUI debugger | GUI debugger | No | No | No | **Yes (programmatic)** |
| VCD waveform export | Yes | Yes | Yes | Yes | No | **Yes** |
| SystemVerilog codegen | N/A (synth input) | N/A | No | No | No | **Yes** |
| TMR/safety insertion | No | No | No | No | No | **Yes (automatic)** |
| Price | Free (vendor lock-in) | $10-50k/seat/yr | Free | Free | $1-5k/yr | **Free (open source)** |

### The gap we fill

**For VHDL engineers in aerospace, defense, automotive, and space:**
These industries standardize on VHDL. Their engineers cannot switch to
SystemVerilog or a new HDL. But they need formal verification (DO-254),
safety analysis (ISO 26262), and CDC checking — tools that currently
cost $100k+ per seat from Synopsys/Cadence/Siemens. skalp gives them
all of this for their existing VHDL code, for free.

**For FPGA engineers and hobbyists:**
ModelSim licenses are disappearing from free Quartus/Vivado bundles.
GHDL and NVC are excellent but they're just simulators — you still
need separate tools for formal, CDC, and linting. skalp is one tool
that does everything. Write VHDL, test in Rust, simulate at compiled
speed, formally verify, and get a language server — all from `cargo test`.

**For teams evaluating skalp (the language):**
The VHDL frontend removes the adoption barrier entirely. Teams can
start by pointing skalp at their existing VHDL codebase to get
simulation, formal, and IDE features. New modules can be written in
skalp. Both languages produce the same HIR and coexist in the same
project. Migration is gradual, not all-or-nothing.

### First-mover advantages

1. **First open-source tool with full synthesizable VHDL-2019.** No
   commercial or open-source tool fully supports VHDL-2019 interfaces
   and mode views for synthesis. We would be the reference implementation.

2. **First tool to unify VHDL simulation and formal verification.**
   Today these require separate tools from separate vendors with separate
   licenses. skalp does both from one HIR.

3. **First tool to offer compiled-C++ VHDL simulation.** GHDL and NVC
   interpret or use LLVM JIT. skalp compiles to native C++ via SIR,
   which is faster for large designs and enables GPU acceleration.

4. **First VHDL language server with full semantic analysis.** Sigasi
   has a commercial LS. GHDL has a basic one. skalp's LS would have
   type-aware completions, cross-language go-to-definition (VHDL entity
   instantiated in skalp or vice versa), and integrated diagnostics
   from formal and CDC analysis — features no existing LS offers.

---

## Pipeline

```
                        ┌──────────────────┐
  .sk files ──────────► │  skalp-frontend  │──┐
                        └──────────────────┘  │
                                              ▼
                        ┌──────────────────┐  ┌─────┐    ┌─────┐    ┌─────┐
  .vhd files ─────────► │   skalp-vhdl     │──► HIR ├───► MIR ├───► SIR ├──► sim / codegen / formal
                        └──────────────────┘  └─────┘    └─────┘    └─────┘
                                              ▲
  .sv files (future) ─► │  skalp-sv        │──┘
```

Each frontend is a separate crate that produces `Hir`. The rest of the
pipeline (`lower_to_mir`, `mir_to_sir`, codegen, simulation, formal) is
shared.

---

## Crate: `skalp-vhdl`

```
crates/skalp-vhdl/
  Cargo.toml
  src/
    lib.rs              # Public API
    lexer.rs            # Logos-based VHDL lexer
    syntax.rs           # SyntaxKind enum (rowan node types)
    parse.rs            # Rowan-based recursive descent parser
    hir_lower.rs        # Syntax tree → skalp HIR
    vhdl_types.rs       # VHDL type system → HirType mapping
    builtins.rs         # ieee.std_logic_1164, numeric_std, math_real
    module_resolver.rs  # library/use clause resolution
    diagnostics.rs      # Error types with source spans
  tests/
    parse_tests.rs
    lower_tests.rs
    integration_tests.rs
```

Dependencies:
```toml
[dependencies]
skalp-frontend = { path = "../skalp-frontend" }  # Reuse Hir types
logos = "0.14"
rowan = "0.15"
indexmap = "2"
```

---

## 1. Lexer (`lexer.rs`)

VHDL is **case-insensitive**. The lexer normalizes all identifiers and
keywords to lowercase during tokenization. This is the single biggest
difference from the skalp lexer.

```rust
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n]+")]           // whitespace
#[logos(skip r"--[^\n]*")]             // VHDL line comments
pub enum Token {
    // --- Keywords (case-insensitive via logos callback) ---
    #[token("entity", ignore(case))]     Entity,
    #[token("architecture", ignore(case))] Architecture,
    #[token("of", ignore(case))]         Of,
    #[token("is", ignore(case))]         Is,
    #[token("begin", ignore(case))]      Begin,
    #[token("end", ignore(case))]        End,
    #[token("port", ignore(case))]       Port,
    #[token("generic", ignore(case))]    Generic,
    #[token("signal", ignore(case))]     Signal,
    #[token("variable", ignore(case))]   Variable,
    #[token("constant", ignore(case))]   Constant,
    #[token("process", ignore(case))]    Process,
    #[token("if", ignore(case))]         If,
    #[token("then", ignore(case))]       Then,
    #[token("elsif", ignore(case))]      Elsif,
    #[token("else", ignore(case))]       Else,
    #[token("case", ignore(case))]       Case,
    #[token("when", ignore(case))]       When,
    #[token("for", ignore(case))]        For,
    #[token("while", ignore(case))]      While,
    #[token("loop", ignore(case))]       Loop,
    #[token("generate", ignore(case))]   Generate,
    #[token("component", ignore(case))]  Component,
    #[token("port map", ignore(case))]   PortMap,
    #[token("generic map", ignore(case))] GenericMap,
    #[token("map", ignore(case))]        Map,
    #[token("library", ignore(case))]    Library,
    #[token("use", ignore(case))]        Use,
    #[token("all", ignore(case))]        All,
    #[token("type", ignore(case))]       Type,
    #[token("subtype", ignore(case))]    Subtype,
    #[token("record", ignore(case))]     Record,
    #[token("array", ignore(case))]      Array,
    #[token("range", ignore(case))]      Range,
    #[token("to", ignore(case))]         To,
    #[token("downto", ignore(case))]     Downto,
    #[token("in", ignore(case))]         In,
    #[token("out", ignore(case))]        Out,
    #[token("inout", ignore(case))]      Inout,
    #[token("buffer", ignore(case))]     Buffer,
    #[token("others", ignore(case))]     Others,
    #[token("null", ignore(case))]       Null,
    #[token("return", ignore(case))]     Return,
    #[token("function", ignore(case))]   Function,
    #[token("procedure", ignore(case))]  Procedure,
    #[token("package", ignore(case))]    Package,
    #[token("body", ignore(case))]       Body,
    #[token("attribute", ignore(case))]  Attribute,
    #[token("alias", ignore(case))]      Alias,
    #[token("assert", ignore(case))]     Assert,
    #[token("report", ignore(case))]     Report,
    #[token("severity", ignore(case))]   Severity,

    // VHDL-2008
    #[token("force", ignore(case))]      Force,
    #[token("release", ignore(case))]    Release,

    // VHDL-2019 (the differentiator)
    #[token("interface", ignore(case))]  Interface,
    #[token("view", ignore(case))]       View,
    #[token("private", ignore(case))]    Private,

    // --- Operators ---
    #[token("<=")]  SignalAssign,    // signal assignment
    #[token(":=")]  VarAssign,       // variable assignment
    #[token("=>")]  Arrow,           // association
    #[token("**")]  DoubleStar,      // exponentiation
    #[token("/=")]  NotEqual,
    #[token(">=")]  Gte,
    #[token("<<")]  DoubleLAngle,    // external names (VHDL-2008)
    #[token(">>")]  DoubleRAngle,
    #[token("<")]   Lt,
    #[token(">")]   Gt,
    #[token("=")]   Eq,
    #[token("+")]   Plus,
    #[token("-")]   Minus,
    #[token("*")]   Star,
    #[token("/")]   Slash,
    #[token("&")]   Ampersand,       // concatenation
    #[token("(")]   LParen,
    #[token(")")]   RParen,
    #[token(":")]   Colon,
    #[token(";")]   Semicolon,
    #[token(",")]   Comma,
    #[token(".")]   Dot,
    #[token("'")]   Tick,            // attribute access / qualified expr
    #[token("|")]   Bar,             // choices separator

    // Logical operators (keywords in VHDL)
    #[token("and", ignore(case))]    And,
    #[token("or", ignore(case))]     Or,
    #[token("nand", ignore(case))]   Nand,
    #[token("nor", ignore(case))]    Nor,
    #[token("xor", ignore(case))]    Xor,
    #[token("xnor", ignore(case))]   Xnor,
    #[token("not", ignore(case))]    Not,
    #[token("mod", ignore(case))]    Mod,
    #[token("rem", ignore(case))]    Rem,
    #[token("abs", ignore(case))]    Abs,
    #[token("sll", ignore(case))]    Sll,
    #[token("srl", ignore(case))]    Srl,
    #[token("sla", ignore(case))]    Sla,
    #[token("sra", ignore(case))]    Sra,
    #[token("rol", ignore(case))]    Rol,
    #[token("ror", ignore(case))]    Ror,

    // --- Literals ---
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_ascii_lowercase())]
    Identifier(String),

    #[regex(r"[0-9][0-9_]*", |lex| parse_integer(lex.slice()))]
    IntegerLiteral(u64),

    // Bit string literals: B"1010", X"FF", O"77"
    #[regex(r#"[bBoOxX]"[0-9a-fA-F_]+""#, |lex| parse_bit_string(lex.slice()))]
    BitStringLiteral(String),

    // Character literal: '1', '0', 'Z'
    #[regex(r"'[^']'", |lex| lex.slice().chars().nth(1).unwrap())]
    CharLiteral(char),

    // String literal: "hello"
    #[regex(r#""[^"]*""#, |lex| lex.slice()[1..lex.slice().len()-1].to_owned())]
    StringLiteral(String),

    // Based literals: 16#FF#, 2#1010#
    #[regex(r"[0-9]+#[0-9a-fA-F_]+#", |lex| parse_based_literal(lex.slice()))]
    BasedLiteral(u64),

    // Real literals: 1.0, 3.14
    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", |lex| parse_real(lex.slice()))]
    RealLiteral(f64),

    Error,
}
```

~120 keyword/operator variants. Much smaller than skalp's ~350 because
VHDL's operator set is simpler and we omit non-synthesizable keywords.

---

## 2. Syntax Kinds (`syntax.rs`)

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // === Tokens (from Token enum) ===
    EntityKw, ArchitectureKw, OfKw, IsKw, BeginKw, EndKw,
    PortKw, GenericKw, SignalKw, VariableKw, ConstantKw,
    ProcessKw, IfKw, ThenKw, ElsifKw, ElseKw, CaseKw, WhenKw,
    ForKw, WhileKw, LoopKw, GenerateKw, ComponentKw,
    LibraryKw, UseKw, AllKw, TypeKw, SubtypeKw,
    RecordKw, ArrayKw, RangeKw, ToKw, DowntoKw,
    InKw, OutKw, InoutKw, BufferKw,
    AndKw, OrKw, NandKw, NorKw, XorKw, XnorKw, NotKw,
    // ... remaining keyword tokens

    // Operators
    SignalAssign, VarAssign, Arrow, DoubleStar,
    NotEqual, Gte, Lt, Gt, Eq,
    Plus, Minus, Star, Slash, Ampersand,
    LParen, RParen, Colon, Semicolon, Comma, Dot, Tick, Bar,

    // Literals
    Ident, IntLiteral, BitStringLiteral, CharLiteral,
    StringLiteral, BasedLiteral, RealLiteral,

    // Trivia
    Whitespace, Comment,
    Error,

    // === Non-terminal (syntax tree nodes) ===
    // Top-level
    SourceFile,
    LibraryClause,          // library ieee;
    UseClause,              // use ieee.std_logic_1164.all;

    // Design units
    EntityDecl,             // entity ... is ... end entity;
    ArchitectureBody,       // architecture ... of ... is ... begin ... end;
    PackageDecl,            // package ... is ... end package;
    PackageBody,            // package body ... is ... end;

    // Entity internals
    PortClause,             // port ( ... );
    PortDecl,               // signal_name : direction type
    GenericClause,          // generic ( ... );
    GenericDecl,            // NAME : type := default

    // Architecture internals
    SignalDecl,             // signal name : type;
    ConstantDecl,           // constant name : type := value;
    VariableDecl,           // variable name : type;
    AliasDecl,              // alias name : type is ...;
    AttributeDecl,          // attribute name : type;
    AttributeSpec,          // attribute name of ... is ...;
    ComponentDecl,          // component ... is ... end component;

    // Type declarations
    TypeDecl,               // type name is ...;
    SubtypeDecl,            // subtype name is ...;
    EnumTypeDef,            // (A, B, C)
    RecordTypeDef,          // record ... end record;
    RecordField,
    ArrayTypeDef,           // array (range) of type
    ConstrainedArrayDef,    // array (0 to N) of type
    UnconstrainedArrayDef,  // array (natural range <>) of type

    // Concurrent statements (in architecture body)
    ConcurrentSignalAssign, // a <= b and c;
    ConditionalAssign,      // a <= b when c = '1' else d;
    SelectedAssign,         // with sel select a <= ...;
    ProcessStmt,            // process ... begin ... end process;
    ComponentInst,          // label : entity work.Foo port map (...);
    ForGenerateStmt,        // for i in 0 to N generate ... end generate;
    IfGenerateStmt,         // if condition generate ... end generate;

    // Sequential statements (in process body)
    SeqSignalAssign,        // a <= b;
    SeqVarAssign,           // a := b;
    IfStmt,                 // if ... then ... elsif ... else ... end if;
    CaseStmt,               // case expr is when ... end case;
    ForLoopStmt,            // for i in range loop ... end loop;
    WhileLoopStmt,          // while condition loop ... end loop;
    NullStmt,               // null;
    ReturnStmt,             // return expr;
    AssertStmt,             // assert condition report "msg" severity ...;
    WaitStmt,               // wait ... (rejected in synth subset)

    // Process internals
    SensitivityList,        // (clk, rst) or (all)

    // Expressions
    BinaryExpr,
    UnaryExpr,
    ParenExpr,
    FunctionCallExpr,
    QualifiedExpr,          // type'(expr)
    TypeConversion,         // unsigned(x)
    AttributeExpr,          // signal'event, signal'range
    AggregateExpr,          // (0 => '1', others => '0')
    IndexExpr,              // a(i)
    SliceExpr,              // a(7 downto 0)
    FieldAccessExpr,        // record.field
    ConcatExpr,             // a & b
    NameExpr,               // simple name reference
    LiteralExpr,

    // Choices / associations
    ChoiceList,             // when A | B | C =>
    ElementAssoc,           // 0 => '1'
    PortMapAspect,          // port map ( ... )
    GenericMapAspect,       // generic map ( ... )
    AssociationList,        // (a => b, c => d)
    NamedAssoc,             // port_name => expr
    PositionalAssoc,        // expr (by position)

    // Subtype indication
    SubtypeIndication,      // std_logic_vector(7 downto 0)
    RangeConstraint,        // (7 downto 0) or (0 to 15)
    DiscreteRange,          // 0 to N, natural range <>

    // Functions / procedures (synthesizable subset)
    FunctionDecl,
    FunctionBody,
    ProcedureDecl,
    ProcedureBody,
    ParamList,
    ParamDecl,

    // VHDL-2019
    InterfaceDecl,          // interface ... is ... end interface;
    ViewDecl,               // view ... of ... is ... end view;
}
```

~150 variants total. Much fewer than skalp's 426 because we target only
the synthesizable subset.

---

## 3. Parser (`parse.rs`)

Same rowan GreenNodeBuilder pattern as skalp-frontend. Recursive descent,
error-recovering.

```rust
pub struct ParseState<'a> {
    tokens: Vec<TokenWithPos>,
    current: usize,
    builder: GreenNodeBuilder<'static>,
    source: &'a str,
    errors: Vec<ParseError>,
}

pub fn parse(source: &str) -> SyntaxNode { ... }
pub fn parse_with_errors(source: &str) -> (SyntaxNode, Vec<ParseError>) { ... }
```

### Grammar sketch (synthesizable subset)

```
source_file     = { library_clause | use_clause | design_unit }*
design_unit     = entity_decl | architecture_body | package_decl | package_body

entity_decl     = "entity" IDENT "is"
                    [ generic_clause ]
                    [ port_clause ]
                  "end" [ "entity" ] [ IDENT ] ";"

architecture_body = "architecture" IDENT "of" IDENT "is"
                      { arch_decl_item }*
                    "begin"
                      { concurrent_stmt }*
                    "end" [ "architecture" ] [ IDENT ] ";"

arch_decl_item  = signal_decl | constant_decl | component_decl
                | type_decl | subtype_decl | alias_decl
                | function_decl | function_body
                | attribute_decl | attribute_spec

port_clause     = "port" "(" port_decl { ";" port_decl }* ")" ";"
port_decl       = ident_list ":" direction subtype_indication [ ":=" expr ]

generic_clause  = "generic" "(" generic_decl { ";" generic_decl }* ")" ";"
generic_decl    = ident_list ":" subtype_indication [ ":=" expr ]

-- Concurrent statements (architecture body)
concurrent_stmt = process_stmt
                | component_inst
                | concurrent_signal_assign
                | conditional_assign
                | selected_assign
                | for_generate_stmt
                | if_generate_stmt

process_stmt    = [ IDENT ":" ] "process" [ "(" sensitivity_list ")" ]
                    [ "is" ] { process_decl }*
                  "begin"
                    { sequential_stmt }*
                  "end" "process" [ IDENT ] ";"

-- Sequential statements (process body)
sequential_stmt = signal_assign_stmt       -- target <= expr;
                | variable_assign_stmt     -- target := expr;
                | if_stmt
                | case_stmt
                | for_loop_stmt
                | while_loop_stmt
                | null_stmt
                | return_stmt
                | assert_stmt
                | function_call_stmt

if_stmt         = "if" expr "then"
                    { sequential_stmt }*
                  { "elsif" expr "then" { sequential_stmt }* }*
                  [ "else" { sequential_stmt }* ]
                  "end" "if" ";"

case_stmt       = "case" expr "is"
                    { "when" choices "=>" { sequential_stmt }* }+
                  "end" "case" ";"

-- Component instantiation (3 forms)
component_inst  = IDENT ":" "entity" selected_name
                    [ "generic" "map" "(" assoc_list ")" ]
                    "port" "map" "(" assoc_list ")" ";"
                -- or: IDENT ":" "component" IDENT ...
                -- or: IDENT ":" IDENT port map (...)  (direct)

-- Types
type_decl       = "type" IDENT "is" type_def ";"
type_def        = enum_type_def | record_type_def | array_type_def
enum_type_def   = "(" IDENT { "," IDENT }* ")"
record_type_def = "record" { IDENT ":" subtype_indication ";" }+ "end" "record"
array_type_def  = "array" "(" discrete_range ")" "of" subtype_indication

subtype_indication = type_mark [ constraint ]
constraint         = "(" range ")"  -- e.g., (7 downto 0)
range              = expr ("to" | "downto") expr
```

### Synthesizable subset: what we reject at parse time

| Rejected construct | Reason |
|---|---|
| `wait` statements | Not synthesizable (use `process(clk)` instead) |
| `after` clauses (`<= X after 10 ns`) | Simulation-only timing |
| `file` declarations and I/O | Not synthesizable |
| `access` types (pointers) | Not synthesizable |
| `shared variable` | Not synthesizable |
| `disconnect` / `guard` | Not synthesizable |
| Physical type declarations | Not synthesizable (except `time` in generics) |
| `transport` / `reject` delay models | Simulation-only |

These produce a clear error: "this construct is not synthesizable; skalp
targets RTL synthesis. Use the Rust testbench API for simulation-only
behavior."

---

## 4. HIR Lowering (`hir_lower.rs`)

This is the core of the frontend. Walk the rowan syntax tree, produce
skalp `Hir` structures.

### Entity / Architecture → HirEntity + HirImplementation

```
VHDL                              skalp HIR
────                              ─────────
entity Foo is                     HirEntity {
  generic (                         generics: [
    WIDTH : natural := 8              HirGeneric { name: "width", ... }
  );                                ],
  port (                            ports: [
    clk   : in  std_logic;            HirPort { name: "clk", dir: Input, type: Clock }
    rst   : in  std_logic;            HirPort { name: "rst", dir: Input, type: Reset }
    d_in  : in  slv(W-1 downto 0);   HirPort { name: "d_in", dir: Input, type: Bit(W) }
    d_out : out slv(W-1 downto 0);   HirPort { name: "d_out", dir: Output, type: Bit(W) }
  );                                ]
end entity;                       }

architecture rtl of Foo is        HirImplementation {
  signal count : unsigned(7..0);    entity: EntityId(Foo),
begin                               signals: [HirSignal { name: "count", type: Nat(8) }],
  process(clk)                      event_blocks: [
  begin                               HirEventBlock {
    if rising_edge(clk) then             triggers: [Edge(clk, Rising)],
      if rst = '1' then                  statements: [
        count <= (others => '0');           If { cond: rst==1, then: [count=0],
      else                                       else: [count=count+1] }
        count <= count + 1;             ]
      end if;                         }
    end if;                         ],
  end process;                      assignments: [
                                      HirAssignment { lhs: d_out, rhs: count }
  d_out <= std_logic_vector(count);  ]
end architecture;                 }
```

### Type Mapping (`vhdl_types.rs`)

| VHDL type | HirType | Notes |
|---|---|---|
| `std_logic` | `Logic(1)` | 4-state: 0, 1, X, Z |
| `std_ulogic` | `Logic(1)` | Unresolved — same for synthesis |
| `std_logic_vector(N-1 downto 0)` | `Logic(N)` | Or `Bit(N)` if provably 2-state |
| `unsigned(N-1 downto 0)` | `Nat(N)` | |
| `signed(N-1 downto 0)` | `Int(N)` | |
| `boolean` | `Bool` | |
| `integer range 0 to M` | `Nat(clog2(M+1))` | Infer width from range |
| `integer range -N to M` | `Int(clog2(max(N,M)+1)+1)` | Signed, infer width |
| `natural` / `positive` | `Nat(32)` | Default 32-bit, narrow from context |
| `bit` | `Bit(1)` | 2-state |
| `bit_vector(N-1 downto 0)` | `Bit(N)` | |
| `type T is (A, B, C)` | `Enum { variants: [A,B,C], width: clog2(3) }` | |
| `type T is record ... end record` | `Struct { fields: [...] }` | |
| `type T is array (0 to N) of ET` | `Array(ET, N+1)` | Constrained |
| `type T is array (natural range <>) of ET` | `Array(ET, param)` | Generic — size from context |

### Clock/Reset Detection

VHDL doesn't have dedicated clock/reset types. We infer them:

```
-- Clock: port used in rising_edge() or falling_edge()
if rising_edge(clk) then ...    →  clk is Clock, edge = Rising

-- Reset: port used as synchronous reset pattern
if rst = '1' then              →  rst is Reset(active_high=true)
  ... reset assignments ...
```

The lowering pass scans process bodies for these patterns:

1. Find `if rising_edge(X)` or `if falling_edge(X)` → X is the clock
2. Inside that block, find `if R = '1' then <reset-assignments>` → R is sync reset
3. Find `if R = '1' then <reset> elsif rising_edge(X)` → R is async reset

This produces proper `HirEventBlock` triggers with clock domain info.

### Process → HirEventBlock

```
-- Clocked process
process(clk)                    →  HirEventBlock {
begin                                triggers: [Edge("clk", Rising)],
  if rising_edge(clk) then          statements: [body...]
    ...                            }
  end if;
end process;

-- Combinational process
process(all)                    →  HirEventBlock {
begin                                triggers: [], -- combinational
  case sel is                        statements: [body...]
    when "00" => y <= a;           }
    when others => y <= b;
  end case;
end process;

-- Concurrent signal assignment
y <= a when sel = '1' else b;   →  HirAssignment {
                                     lhs: y,
                                     type: Combinational,
                                     rhs: Conditional(sel==1, a, b)
                                   }
```

### Signal vs Variable Semantics

```
signal s : ...;     -- delayed update    → HirSignal   (NonBlocking assignments)
variable v : ...;   -- immediate update  → HirVariable (Blocking assignments)

s <= expr;          → HirAssignment { type: NonBlocking, ... }
v := expr;          → HirAssignment { type: Blocking, ... }
```

This maps perfectly to skalp's model. No semantic transformation needed.

### Component Instantiation → HirInstance

```
-- VHDL direct entity instantiation
u_fifo : entity work.FIFO
  generic map (
    DEPTH => 16,
    WIDTH => 8
  )
  port map (
    clk     => clk,
    wr_en   => fifo_wr,
    wr_data => fifo_din,
    rd_en   => fifo_rd,
    rd_data => fifo_dout,
    full    => fifo_full,
    empty   => fifo_empty
  );

-- Maps to:
HirInstance {
    name: "u_fifo",
    entity: EntityId(FIFO),
    named_generic_args: { "depth": Literal(16), "width": Literal(8) },
    connections: [
        HirConnection { port: "clk",     expr: Signal("clk") },
        HirConnection { port: "wr_en",   expr: Signal("fifo_wr") },
        HirConnection { port: "wr_data", expr: Signal("fifo_din") },
        ...
    ]
}
```

### Generate Statements

```
-- for-generate → unrolled HirInstances
gen_adders : for i in 0 to 3 generate
  u_add : entity work.Adder
    port map (a => a(i), b => b(i), s => s(i));
end generate;

-- Unrolled to 4 HirInstances: u_add_0, u_add_1, u_add_2, u_add_3
-- with connections indexed by the generate index.

-- if-generate → conditional instantiation (lowered in MIR)
gen_parity : if PARITY_EN generate
  u_par : entity work.ParityChecker ...;
end generate;
```

### VHDL-2019 Interfaces → Struct-like Ports

```vhdl
-- VHDL-2019 interface declaration
interface axi_lite is
  signal awaddr  : std_logic_vector(31 downto 0);
  signal awvalid : std_logic;
  signal awready : std_logic;
  signal wdata   : std_logic_vector(31 downto 0);
  -- ...
end interface;

view axi_master of axi_lite is
  awaddr  : out;
  awvalid : out;
  awready : in;
  wdata   : out;
end view;

-- Usage in entity
entity peripheral is
  port (
    clk  : in std_logic;
    bus  : view axi_master  -- interface port with direction view
  );
end entity;
```

This maps to skalp's struct-typed ports with directional fields — a
natural fit for `HirStructType` with per-field `HirPortDirection`.

---

## 5. Built-in Libraries (`builtins.rs`)

We don't parse `ieee.std_logic_1164` — we provide built-in type
definitions and function signatures.

```rust
/// Built-in VHDL types that map directly to HirType.
/// No need to parse the IEEE library source.
pub fn resolve_builtin_type(lib: &str, pkg: &str, name: &str) -> Option<HirType> {
    match (lib, pkg, name) {
        // ieee.std_logic_1164
        ("ieee", "std_logic_1164", "std_logic")        => Some(HirType::Logic(1)),
        ("ieee", "std_logic_1164", "std_ulogic")       => Some(HirType::Logic(1)),
        ("ieee", "std_logic_1164", "std_logic_vector")  => Some(HirType::Logic(0)), // width from constraint
        ("ieee", "std_logic_1164", "std_ulogic_vector") => Some(HirType::Logic(0)),

        // ieee.numeric_std
        ("ieee", "numeric_std", "unsigned")             => Some(HirType::Nat(0)),
        ("ieee", "numeric_std", "signed")               => Some(HirType::Int(0)),

        // std.standard (implicit)
        ("std", "standard", "boolean")                  => Some(HirType::Bool),
        ("std", "standard", "integer")                  => Some(HirType::Int(32)),
        ("std", "standard", "natural")                  => Some(HirType::Nat(32)),
        ("std", "standard", "positive")                 => Some(HirType::Nat(32)),
        ("std", "standard", "bit")                      => Some(HirType::Bit(1)),
        ("std", "standard", "bit_vector")               => Some(HirType::Bit(0)),

        _ => None,
    }
}

/// Built-in function signatures for type conversion and arithmetic.
pub fn resolve_builtin_function(name: &str) -> Option<BuiltinFn> {
    match name {
        "rising_edge"          => Some(BuiltinFn::RisingEdge),
        "falling_edge"         => Some(BuiltinFn::FallingEdge),
        "to_unsigned"          => Some(BuiltinFn::ToUnsigned),
        "to_signed"            => Some(BuiltinFn::ToSigned),
        "to_integer"           => Some(BuiltinFn::ToInteger),
        "unsigned"             => Some(BuiltinFn::CastUnsigned),  // type conversion
        "signed"               => Some(BuiltinFn::CastSigned),
        "std_logic_vector"     => Some(BuiltinFn::CastSlv),
        "resize"               => Some(BuiltinFn::Resize),
        "shift_left"           => Some(BuiltinFn::ShiftLeft),
        "shift_right"          => Some(BuiltinFn::ShiftRight),
        "to_01"                => Some(BuiltinFn::To01),          // resolve metavalues
        _                      => None,
    }
}
```

---

## 6. Module Resolution (`module_resolver.rs`)

VHDL's library/use system maps to file lookup:

```
library ieee;                       → built-in (builtins.rs)
use ieee.std_logic_1164.all;        → built-in

library work;                       → current project directory
use work.fifo_pkg.all;              → look for fifo_pkg.vhd in search paths

library mylib;                      → look up in vhdl.toml or skalp.toml [vhdl.libraries]
use mylib.types_pkg.all;            → resolve from configured library path
```

Search order:
1. Built-in libraries (`ieee`, `std`)
2. `work` library (current project directory)
3. Named libraries from config (`skalp.toml` `[vhdl.libraries]` section)

File extensions: `.vhd`, `.vhdl`, `.v2k8`, `.v2k19`

---

## 7. Public API (`lib.rs`)

```rust
pub use skalp_frontend::hir::Hir;  // Reuse the same HIR type

pub struct VhdlCompilationContext {
    pub main_hir: Hir,
    pub package_hirs: IndexMap<String, Hir>,  // library.package → HIR
}

/// Parse a VHDL file and produce skalp HIR.
pub fn parse_vhdl(file_path: &Path) -> Result<VhdlCompilationContext> {
    // 1. Read source
    // 2. Lex (logos)
    // 3. Parse (rowan → SyntaxNode)
    // 4. Resolve library/use (builtins + file lookup)
    // 5. Lower to HIR (hir_lower.rs)
    // 6. Return compilation context
}

/// Parse VHDL source string (for testing / IDE integration).
pub fn parse_vhdl_source(source: &str) -> Result<Hir> { ... }
```

Integration with the main skalp pipeline:

```rust
// In skalp-mir or the top-level driver:
let context = if path.extension() == Some("vhd") {
    let vhdl_ctx = skalp_vhdl::parse_vhdl(&path)?;
    CompilationContext {
        main_hir: vhdl_ctx.main_hir,
        module_hirs: vhdl_ctx.package_hirs,
    }
} else {
    skalp_frontend::parse_and_build_compilation_context(&path)?
};

let mir = skalp_mir::lower_to_mir(&context.main_hir)?;
// ... rest of pipeline unchanged
```

---

## 8. Skalp-Specific Features in VHDL

Skalp has features with no VHDL equivalent: intents, protocols,
requirements, safety annotations, trace/breakpoint debugging, clock
domain lifetimes, and NCL. These need a way to be expressed in VHDL
source without breaking portability to other tools.

### Approach: `-- skalp:` comment pragmas

Structured comment pragmas are the primary mechanism. They sit on the
line immediately above (or beside) the construct they annotate. The
VHDL remains valid and portable — other tools ignore comments. The
skalp VHDL parser strips the `-- skalp:` prefix and parses the rest
using the same grammar as skalp's `#[...]` attribute syntax.

This follows established VHDL practice: `-- synthesis translate_off`,
`-- pragma translate_off`, `-- synopsys parallel_case` are all
comment-based tool directives that VHDL engineers already understand.

### Feature-by-feature mapping

#### Safety annotations

```vhdl
-- skalp: safety_mechanism(tmr)
entity TmrCounter is
  port (
    clk    : in  std_logic;
    enable : in  std_logic;
    count  : out unsigned(7 downto 0);
    error  : out std_logic  -- skalp: detection_signal
  );
end entity;
```

Lowers to:
```
HirEntity {
    safety_config: Some(SafetyConfig { mechanism: Tmr }),
    ports: [
        ...,
        HirPort { name: "error", detection_config: Some(DetectionConfig { ... }) }
    ]
}
```

#### Intents (formal property generation)

```vhdl
-- skalp: intent "the FIFO count never exceeds DEPTH"
-- skalp: intent "after reset, all pointers are zero"
entity FIFO is
  ...
end entity;
```

Single-line intents work as pragmas. For multi-line intents with
explicit formal properties, reference a skalp file:

```vhdl
-- skalp: intent_file("fifo_intents.sk")
```

#### Trace and breakpoint debugging

```vhdl
architecture rtl of UartRx is
  signal shift_reg : std_logic_vector(7 downto 0);  -- skalp: trace(group = "datapath")
  signal bit_index : unsigned(2 downto 0);           -- skalp: trace(group = "datapath")
  signal baud_cnt  : unsigned(8 downto 0);           -- skalp: trace(group = "timing")
begin

  -- skalp: breakpoint(shift_reg = x"FF")
  -- skalp: breakpoint(baud_cnt > 434)
  process(clk)
  begin
    ...
  end process;

end architecture;
```

These map directly to `HirSignal.trace_config` and
`HirFormalBlock.breakpoint_conditions` in HIR.

#### Clock domains — type-based enforcement (no pragmas)

VHDL's nominal type system can enforce CDC safety at compile time —
the VHDL compiler itself rejects cross-domain assignments, before
skalp even sees the code. This is the VHDL equivalent of skalp's
clock lifetime parameters.

**The mechanism:** two array (or enumeration) type declarations with
the same structure are *incompatible types* in VHDL. Define a distinct
type per clock domain. Assignments between domains require explicit
type conversion — and that conversion lives inside the synchronizer.

skalp provides a generic package that makes this ergonomic:

```vhdl
-- Provided by skalp as a standard library package
package skalp_clock_domain is
  generic (DOMAIN : string);

  -- Distinct enumeration — NOT a subtype of std_logic.
  -- Subtypes are assignment-compatible with their parent;
  -- a new enumeration type is not.
  type domain_bit is ('0', '1');

  -- Arrays of domain_bit — also distinct per instantiation
  type domain_vec is array (natural range <>) of domain_bit;
  type domain_unsigned is array (natural range <>) of domain_bit;
  type domain_signed is array (natural range <>) of domain_bit;

  -- Conversion functions at domain boundaries
  function to_std_logic(b : domain_bit) return std_logic;
  function from_std_logic(s : std_logic) return domain_bit;
  function to_slv(v : domain_vec) return std_logic_vector;
  function from_slv(s : std_logic_vector) return domain_vec;
end package;
```

Users instantiate the package once per clock domain:

```vhdl
package sys is new skalp_clock_domain generic map (DOMAIN => "sys");
package uart is new skalp_clock_domain generic map (DOMAIN => "uart");
```

All signals declare their domain through their type:

```vhdl
use work.sys;
use work.uart;

architecture rtl of UartTop is
  signal sys_count  : sys.domain_unsigned(7 downto 0);
  signal sys_valid  : sys.domain_bit;
  signal uart_data  : uart.domain_vec(7 downto 0);
  signal uart_ready : uart.domain_bit;
begin
  -- These are COMPILE ERRORS — VHDL type system rejects them:
  uart_data  <= sys_count;   -- ERROR: sys.domain_unsigned vs uart.domain_vec
  uart_ready <= sys_valid;   -- ERROR: sys.domain_bit vs uart.domain_bit
end architecture;
```

Both vectors and single bits are enforced. The only way to cross
domains is through explicit conversion, which lives in two places:

**1. Top-level ports** — where external `std_logic` pins enter a domain:

```vhdl
entity UartTop is
  port (
    rx : in std_logic   -- external pin, no domain yet
  );
end entity;

architecture rtl of UartTop is
  signal rx_internal : uart.domain_bit;
begin
  rx_internal <= uart.from_std_logic(rx);  -- enters uart domain here
end architecture;
```

**2. Synchronizers** — the one legal crossing point:

```vhdl
entity BitSync is
  port (
    src     : in  sys.domain_bit;
    dst     : out uart.domain_bit;
    dst_clk : in  std_logic
  );
end entity;

architecture rtl of BitSync is
  signal ff0, ff1 : std_logic;  -- internal, untyped
begin
  process(dst_clk)
  begin
    if rising_edge(dst_clk) then
      ff0 <= sys.to_std_logic(src);  -- leave sys domain
      ff1 <= ff0;
    end if;
  end process;
  dst <= uart.from_std_logic(ff1);   -- enter uart domain
end architecture;
```

**Frontend handling:** the skalp VHDL frontend recognizes
`skalp_clock_domain` package instantiations and maps them to HIR:

- Each package instantiation → `HirClockDomain` (name from the
  `DOMAIN` generic string)
- `domain_bit` → `HirType::Bit(1)` tagged with clock domain ID
- `domain_vec` → `HirType::Bit(N)` tagged with clock domain ID
- `domain_unsigned` → `HirType::Nat(N)` tagged with clock domain ID
- `domain_signed` → `HirType::Int(N)` tagged with clock domain ID
- Conversion functions → zero-cost identity operations in synthesis
  (a `domain_bit('0', '1')` is a single wire, same as `std_logic`
  restricted to the synthesizable subset)

skalp's existing CDC analysis then *verifies* what the types already
guarantee — a belt-and-suspenders approach where the VHDL compiler is
the first line of defense and skalp's analysis catches anything the
types missed (e.g., signals that bypass the domain type convention).

**Fallback for untyped designs:** not all VHDL code will use the
domain type convention. For legacy code that uses plain `std_logic`
everywhere, skalp falls back to inference from process sensitivity
lists:

```vhdl
-- No domain types: skalp infers domains from process structure
process(sys_clk)                   -- inferred: clock domain 'sys_clk'
begin
  if rising_edge(sys_clk) then ... end if;
end process;

process(uart_clk)                  -- inferred: clock domain 'uart_clk'
begin
  if rising_edge(uart_clk) then ... end if;
end process;
```

And pragmas remain available for explicit CDC crossing annotations on
legacy code:

```vhdl
-- skalp: cdc(from = "sys_clk", to = "uart_clk")
signal sync_reg : std_logic_vector(1 downto 0);
```

#### Retention (power domains)

```vhdl
signal cal_value : unsigned(15 downto 0);  -- skalp: retention
signal config    : config_t;               -- skalp: retention(domain = "always_on")
```

#### Requirements traceability (DO-254, ISO 26262)

```vhdl
-- skalp: requirement(REQ_UART_001, "Baud rate error shall not exceed 2%")
-- skalp: requirement(REQ_UART_002, "Receiver shall detect framing errors")
entity UartRx is
  ...
end entity;
```

For full requirement specifications with verification links, reference
a skalp file:

```vhdl
-- skalp: requirements_file("uart_requirements.sk")
```

#### Protocols

Protocol specifications are inherently multi-signal, multi-cycle
timing contracts. These are too complex for comment pragmas. Use a
skalp protocol file and reference it:

```vhdl
-- skalp: protocol("spi_protocol.sk", SpiMaster)
entity SpiController is
  port (
    sclk : out std_logic;
    mosi : out std_logic;
    miso : in  std_logic;
    cs_n : out std_logic
  );
end entity;
```

The protocol file is written in skalp's protocol syntax and defines
the timing relationships between the ports. The VHDL frontend resolves
the reference and attaches the protocol to the `HirEntity`.

#### NCL (Null Convention Logic)

NCL is a fundamentally different circuit paradigm — dual-rail encoding,
completion detection, no global clock. There is no sensible way to
express this in VHDL comments. VHDL engineers targeting NCL should
write those modules in skalp directly. The two languages coexist in
the same project:

```
project/
  src/
    datapath.vhd          -- conventional clocked VHDL
    ncl_pipeline.sk        -- NCL module in skalp
    top.vhd               -- instantiates both
```

### Summary: what goes where

| Feature | Mechanism | Example |
|---|---|---|
| Clock domains | **VHDL types** | `sys.domain_vec`, `uart.domain_bit` via `skalp_clock_domain` generic package |
| CDC enforcement | **VHDL type system** | Cross-domain assignment is a VHDL compile error; synchronizers use explicit conversion |
| CDC (legacy code) | Auto-inferred + pragma | Inferred from `process(clk)`, pragma `-- skalp: cdc(from, to)` for crossings |
| Safety annotations | Inline pragma | `-- skalp: safety_mechanism(tmr)` |
| Detection signals | Inline pragma | `-- skalp: detection_signal` |
| Intents (simple) | Inline pragma | `-- skalp: intent "count never overflows"` |
| Intents (complex) | Reference file | `-- skalp: intent_file("intents.sk")` |
| Trace groups | Inline pragma | `-- skalp: trace(group = "datapath")` |
| Breakpoints | Inline pragma | `-- skalp: breakpoint(count = xFF)` |
| Retention | Inline pragma | `-- skalp: retention` |
| Requirements (simple) | Inline pragma | `-- skalp: requirement(REQ_001, "...")` |
| Requirements (full) | Reference file | `-- skalp: requirements_file("reqs.sk")` |
| Protocols | Reference file | `-- skalp: protocol("proto.sk", SpiMaster)` |
| NCL | Write in skalp | Not supported in VHDL — use `.sk` files |

### Parser implementation

The pragma parser is a small addition to the lexer/parser:

```rust
/// Extract skalp pragmas from VHDL comments.
/// Called during lexing — when a comment starting with "-- skalp:" is
/// encountered, it is tokenized as a SkalPragma instead of skipped.
fn try_parse_pragma(comment: &str) -> Option<Pragma> {
    let body = comment.strip_prefix("-- skalp:")?.trim();
    // Reuse skalp-frontend's attribute parser for the body.
    // "safety_mechanism(tmr)" parses the same way as #[safety_mechanism(tmr)]
    skalp_frontend::parse_attribute(body).ok()
}
```

This reuses the existing skalp attribute parser — the pragma body after
`-- skalp:` uses identical syntax to skalp's `#[...]` attributes. No
new grammar to design or maintain.

---

## 9. Implementation Order

### Phase 1: Core (MVP)
Parse and lower the synthesizable subset that covers 80% of real RTL:

1. **Lexer** — all keywords, operators, literals
2. **Parser** — entity, architecture, process, signal/variable assignments,
   if/elsif/else, case/when, component instantiation, for-generate
3. **Type mapping** — std_logic, std_logic_vector, unsigned, signed, integer,
   boolean, enumerations, records, constrained arrays
4. **HIR lowering** — entities, clocked processes, combinational processes,
   concurrent assignments, instances with port/generic maps
5. **Builtins** — ieee.std_logic_1164, ieee.numeric_std, std.standard
6. **Integration test** — parse a FIFO, a counter, a UART receiver in VHDL,
   lower to HIR, compile through MIR/SIR, simulate with Rust testbench

### Phase 2: Full VHDL-2008
7. Packages and package bodies (user-defined types shared across files)
8. Functions and procedures (synthesizable subset)
9. Unconstrained arrays and generic types
10. Selected/conditional signal assignments
11. Aggregates with `others`
12. Attributes (`'range`, `'length`, `'high`, `'low`, `'left`, `'right`, `'event`)
13. Alias declarations
14. `if-generate` with `else generate`

### Phase 3: VHDL-2019 (differentiator)
15. Interface declarations
16. Mode views (directional interfaces)
17. Generic types (fully parameterized packages)
18. Conditional and selected force/release

### Phase 4: Tooling (reuse existing skalp LS)

The existing skalp language server is built on rowan syntax trees and HIR
symbol tables. Since the VHDL frontend produces the same structures, most
LS features work with minimal adaptation:

| Feature | Reuse | VHDL-specific work |
|---|---|---|
| Diagnostics | As-is — operates on `HirError` spans | None |
| Go-to-definition | As-is — resolves through HIR symbol tables | None |
| Find references | As-is — same HIR symbol ID mechanism | None |
| Hover (type info) | As-is — reads `HirType` from symbol table | None |
| Rename | As-is — operates on HIR symbol IDs | None |
| Completions (type-aware) | As-is — port names, signal names, record fields from HIR | None |
| Completions (syntax) | New | VHDL keyword/snippet layer (`process ... begin ... end process;` skeletons, `entity ... is ... end entity;` templates) |
| Formatting | New | VHDL indentation/alignment rules (rowan preserves whitespace, so the formatter walks the lossless tree) |
| Lint | Mostly reuse | CDC analysis, safety checks from HIR. Add VHDL-specific lint (missing sensitivity list entries, incomplete case choices) |
| Signature help | Mostly reuse | VHDL function/procedure signatures from builtins + user packages |

The LS dispatch layer detects file extension (`.vhd`/`.vhdl` vs `.sk`) and
routes to the appropriate frontend for parsing, but all downstream features
(symbol resolution, diagnostics, navigation) are shared.

19. Wire VHDL parser into existing LS dispatch (file extension routing)
20. VHDL syntax completions and snippets
21. VHDL formatter (rowan lossless tree walker)
22. VHDL-specific lint rules

---

## 10. Example: VHDL FIFO → skalp simulation

```vhdl
-- fifo.vhd
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity FIFO is
  generic (
    DEPTH : natural := 16;
    WIDTH : natural := 8
  );
  port (
    clk     : in  std_logic;
    rst     : in  std_logic;
    wr_en   : in  std_logic;
    wr_data : in  std_logic_vector(WIDTH-1 downto 0);
    rd_en   : in  std_logic;
    rd_data : out std_logic_vector(WIDTH-1 downto 0);
    full    : out std_logic;
    empty   : out std_logic
  );
end entity;

architecture rtl of FIFO is
  type mem_t is array (0 to DEPTH-1) of std_logic_vector(WIDTH-1 downto 0);
  signal memory : mem_t;
  signal wr_ptr : unsigned(3 downto 0) := (others => '0');
  signal rd_ptr : unsigned(3 downto 0) := (others => '0');
  signal count  : unsigned(4 downto 0) := (others => '0');
begin

  rd_data <= memory(to_integer(rd_ptr));
  full    <= '1' when count = DEPTH else '0';
  empty   <= '1' when count = 0     else '0';

  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        wr_ptr <= (others => '0');
        rd_ptr <= (others => '0');
        count  <= (others => '0');
      else
        if wr_en = '1' and count < DEPTH then
          memory(to_integer(wr_ptr)) <= wr_data;
          wr_ptr <= (wr_ptr + 1) mod DEPTH;
          count  <= count + 1;
        end if;
        if rd_en = '1' and count > 0 then
          rd_ptr <= (rd_ptr + 1) mod DEPTH;
          count  <= count - 1;
        end if;
      end if;
    end if;
  end process;

end architecture;
```

Test with the same Rust testbench:

```rust
use skalp_testing::Testbench;

#[tokio::test]
async fn test_vhdl_fifo_ordering() {
    // Only difference: file extension is .vhd instead of .sk
    let mut tb = Testbench::with_top_module("src/fifo.vhd", "FIFO")
        .await.unwrap();
    tb.reset(2).await;

    // Write "Hello"
    for &byte in &[0x48u32, 0x65, 0x6C, 0x6C, 0x6F] {
        tb.set("wr_en", 1u8);
        tb.set("wr_data", byte);
        tb.clock(1).await;
    }
    tb.set("wr_en", 0u8);

    // Read back in order
    for &expected in &[0x48u32, 0x65, 0x6C, 0x6C, 0x6F] {
        let data = tb.get_u64("rd_data").await as u32;
        assert_eq!(data, expected);
        tb.set("rd_en", 1u8);
        tb.clock(1).await;
        tb.set("rd_en", 0u8);
    }
}
```

The user's workflow: write VHDL as usual, test with Rust, simulate at
compiled-C++ speed, get formal verification and CDC analysis for free.
No new language to learn. No external simulator license.
