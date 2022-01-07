type 'a asm
type text = [ `text ] asm
type data = [ `data ] asm
type label = string

val nop : [> ] asm

val ( ++ ) :
  ([< `data | `text ] as 'a) asm -> 'a asm -> 'a asm

val inline : string -> [> ] asm

type program = { text : text; data : data }

val print_program : Format.formatter -> program -> unit
val print_in_file : file:string -> program -> unit

type size = [ `B | `L | `Q | `W ]
type 'size register

val rax : [ `Q ] register
val rbx : [ `Q ] register
val rcx : [ `Q ] register
val rdx : [ `Q ] register
val rsi : [ `Q ] register
val rdi : [ `Q ] register
val rbp : [ `Q ] register
val rsp : [ `Q ] register
val r8 : [ `Q ] register
val r9 : [ `Q ] register
val r10 : [ `Q ] register
val r11 : [ `Q ] register
val r12 : [ `Q ] register
val r13 : [ `Q ] register
val r14 : [ `Q ] register
val r15 : [ `Q ] register
val eax : [ `L ] register
val ebx : [ `L ] register
val ecx : [ `L ] register
val edx : [ `L ] register
val esi : [ `L ] register
val edi : [ `L ] register
val ebp : [ `L ] register
val esp : [ `L ] register
val r8d : [ `L ] register
val r9d : [ `L ] register
val r10d : [ `L ] register
val r11d : [ `L ] register
val r12d : [ `L ] register
val r13d : [ `L ] register
val r14d : [ `L ] register
val r15d : [ `L ] register
val ax : [ `W ] register
val bx : [ `W ] register
val cx : [ `W ] register
val dx : [ `W ] register
val si : [ `W ] register
val di : [ `W ] register
val bp : [ `W ] register
val sp : [ `W ] register
val r8w : [ `W ] register
val r9w : [ `W ] register
val r10w : [ `W ] register
val r11w : [ `W ] register
val r12w : [ `W ] register
val r13w : [ `W ] register
val r14w : [ `W ] register
val r15w : [ `W ] register
val al : [ `B ] register
val bl : [ `B ] register
val cl : [ `B ] register
val dl : [ `B ] register
val ah : [ `B ] register
val bh : [ `B ] register
val ch : [ `B ] register
val dh : [ `B ] register
val sil : [ `B ] register
val dil : [ `B ] register
val bpl : [ `B ] register
val spl : [ `B ] register
val r8b : [ `B ] register
val r9b : [ `B ] register
val r10b : [ `B ] register
val r11b : [ `B ] register
val r12b : [ `B ] register
val r13b : [ `B ] register
val r14b : [ `B ] register
val r15b : [ `B ] register

type 'size operand

val imm : int -> [> ] operand
val imm32 : int32 -> [> ] operand
val imm64 : int64 -> [> ] operand
val reg : 'size register -> 'size operand
val ( !% ) : 'size register -> 'size operand

val ind :
  ?ofs:int ->
  ?index:'size1 register ->
  ?scale:int ->
  'size2 register ->
  [> ] operand

val lab : label -> [> ] operand
val ilab : label -> [ `Q ] operand
val movb : [ `B ] operand -> [ `B ] operand -> text
val movw : [ `W ] operand -> [ `W ] operand -> text
val movl : [ `L ] operand -> [ `L ] operand -> text
val movq : [ `Q ] operand -> [ `Q ] operand -> text
val movsbw : [ `B ] operand -> [ `W ] register -> text
val movsbl : [ `B ] operand -> [ `L ] register -> text
val movsbq : [ `B ] operand -> [ `Q ] register -> text
val movswl : [ `W ] operand -> [ `L ] register -> text
val movswq : [ `W ] operand -> [ `Q ] register -> text
val movslq : [ `L ] operand -> [ `Q ] register -> text
val movzbw : [ `B ] operand -> [ `W ] register -> text
val movzbl : [ `B ] operand -> [ `L ] register -> text
val movzbq : [ `B ] operand -> [ `Q ] register -> text
val movzwl : [ `W ] operand -> [ `L ] register -> text
val movzwq : [ `W ] operand -> [ `Q ] register -> text
val movabsq : [ `Q ] operand -> [ `Q ] register -> text
val leab : [ `B ] operand -> [ `B ] register -> text
val leaw : [ `W ] operand -> [ `W ] register -> text
val leal : [ `L ] operand -> [ `L ] register -> text
val leaq : [ `Q ] operand -> [ `Q ] register -> text
val incb : [ `B ] operand -> text
val incw : [ `W ] operand -> text
val incl : [ `L ] operand -> text
val incq : [ `Q ] operand -> text
val decb : [ `B ] operand -> text
val decw : [ `W ] operand -> text
val decl : [ `L ] operand -> text
val decq : [ `Q ] operand -> text
val negb : [ `B ] operand -> text
val negw : [ `W ] operand -> text
val negl : [ `L ] operand -> text
val negq : [ `Q ] operand -> text
val addb : [ `B ] operand -> [ `B ] operand -> text
val addw : [ `W ] operand -> [ `W ] operand -> text
val addl : [ `L ] operand -> [ `L ] operand -> text
val addq : [ `Q ] operand -> [ `Q ] operand -> text
val subb : [ `B ] operand -> [ `B ] operand -> text
val subw : [ `W ] operand -> [ `W ] operand -> text
val subl : [ `L ] operand -> [ `L ] operand -> text
val subq : [ `Q ] operand -> [ `Q ] operand -> text
val imulw : [ `W ] operand -> [ `W ] operand -> text
val imull : [ `L ] operand -> [ `L ] operand -> text
val imulq : [ `Q ] operand -> [ `Q ] operand -> text
val idivq : [ `Q ] operand -> text
val cqto : text
val notb : [ `B ] operand -> text
val notw : [ `W ] operand -> text
val notl : [ `L ] operand -> text
val notq : [ `Q ] operand -> text
val andb : [ `B ] operand -> [ `B ] operand -> text
val andw : [ `W ] operand -> [ `W ] operand -> text
val andl : [ `L ] operand -> [ `L ] operand -> text
val andq : [ `Q ] operand -> [ `Q ] operand -> text
val orb : [ `B ] operand -> [ `B ] operand -> text
val orw : [ `W ] operand -> [ `W ] operand -> text
val orl : [ `L ] operand -> [ `L ] operand -> text
val orq : [ `Q ] operand -> [ `Q ] operand -> text
val xorb : [ `B ] operand -> [ `B ] operand -> text
val xorw : [ `W ] operand -> [ `W ] operand -> text
val xorl : [ `L ] operand -> [ `L ] operand -> text
val xorq : [ `Q ] operand -> [ `Q ] operand -> text
val shlb : [ `B ] operand -> [ `B ] operand -> text
val shlw : [ `W ] operand -> [ `W ] operand -> text
val shll : [ `L ] operand -> [ `L ] operand -> text
val shlq : [ `Q ] operand -> [ `Q ] operand -> text
val shrb : [ `B ] operand -> [ `B ] operand -> text
val shrw : [ `W ] operand -> [ `W ] operand -> text
val shrl : [ `L ] operand -> [ `L ] operand -> text
val shrq : [ `Q ] operand -> [ `Q ] operand -> text
val sarb : [ `B ] operand -> [ `B ] operand -> text
val sarw : [ `W ] operand -> [ `W ] operand -> text
val sarl : [ `L ] operand -> [ `L ] operand -> text
val sarq : [ `Q ] operand -> [ `Q ] operand -> text
val call : label -> text
val call_star : [ `Q ] operand -> text
val leave : text
val ret : text
val jmp : label -> text
val jmp_star : [ `Q ] operand -> text
val je : label -> text
val jz : label -> text
val jne : label -> text
val jnz : label -> text
val js : label -> text
val jns : label -> text
val jg : label -> text
val jge : label -> text
val jl : label -> text
val jle : label -> text
val ja : label -> text
val jae : label -> text
val jb : label -> text
val jbe : label -> text
val cmpb : [ `B ] operand -> [ `B ] operand -> text
val cmpw : [ `W ] operand -> [ `W ] operand -> text
val cmpl : [ `L ] operand -> [ `L ] operand -> text
val cmpq : [ `Q ] operand -> [ `Q ] operand -> text
val testb : [ `B ] operand -> [ `B ] operand -> text
val testw : [ `W ] operand -> [ `W ] operand -> text
val testl : [ `L ] operand -> [ `L ] operand -> text
val testq : [ `Q ] operand -> [ `Q ] operand -> text
val sete : [ `B ] operand -> text
val setne : [ `B ] operand -> text
val sets : [ `B ] operand -> text
val setns : [ `B ] operand -> text
val setg : [ `B ] operand -> text
val setge : [ `B ] operand -> text
val setl : [ `B ] operand -> text
val setle : [ `B ] operand -> text
val seta : [ `B ] operand -> text
val setae : [ `B ] operand -> text
val setb : [ `B ] operand -> text
val setbe : [ `B ] operand -> text
val pushq : [ `Q ] operand -> text
val popq : [ `Q ] register -> text
val label : label -> [> ] asm
val globl : label -> [> ] asm
val comment : string -> [> ] asm
val string : string -> data
val dbyte : int list -> data
val dword : int list -> data
val dint : int list -> data
val dquad : int list -> data
val address : label list -> data
val space : int -> data
