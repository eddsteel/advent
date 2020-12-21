datatype Op = Plus of int | Minus of int
datatype Instruction = NOP of Op | ACC of Op | JMP of Op
type State =
     { pc: int, acc: int, instructions: Instruction array,
       visited: Intset.intset, terminated: bool, looped: bool}

fun drops n s = String.implode (List.drop(String.explode s, n))
fun takes n s = String.implode (List.take(String.explode s, n))

fun initState instructions =
    { pc = 0, acc = 0, instructions = Array.fromList(instructions),
      visited = Intset.empty, terminated = false, looped = false}

fun readSigned str = case (takes 1 str, Int.fromString (drops 1 str))
                      of ("-", SOME n) => SOME (Minus n)
                       | ("+", SOME n) => SOME (Plus n)
                       |  _  => NONE

fun readInstruction str =  case (takes 3 str, readSigned (drops 4 str))
                            of ("nop", SOME sign) => SOME (NOP sign)
                             | ("acc", SOME sign) => SOME (ACC sign)
                             | ("jmp", SOME sign) => SOME (JMP sign)
                             | _ =>  NONE

fun currentInstruction (state: State) =
    if  #pc state < Array.length (#instructions state)
    then SOME (Array.sub(#instructions state, #pc state))
    else NONE

fun inc (state: State) = {
        pc = #pc state + 1,
        acc = #acc state,
        instructions = #instructions state,
        visited = #visited state,
        looped = #looped state,
        terminated = #terminated state
    }

fun acc (a, state: State) = {
        pc = #pc state,
        acc = #acc state + a,
        instructions = #instructions state,
        visited = #visited state,
        looped = #looped state,
        terminated = #terminated state
    }

fun dec (d, state: State) = {
        pc = #pc state,
        acc = #acc state - d,
        instructions = #instructions state,
        visited = #visited state,
        looped = #looped state,
        terminated = #terminated state
    }

fun jmp ((Plus n),  state: State) = {
        pc = (#pc state) + n,
        acc = #acc state,
        instructions = #instructions state,
        visited = #visited state,
        looped = #looped state,
        terminated = #terminated state
}
  | jmp ((Minus n), state: State) = {
        pc = #pc state - n,
        acc = #acc state,
        instructions = #instructions state,
        visited = #visited state,
        looped = #looped state,
        terminated = #terminated state
    }

fun visit(state: State) = {
        pc = #pc state,
        acc = #acc state,
        instructions = #instructions state,
        visited = Intset.add(#visited state, #pc state),
        looped = Intset.member(#visited state, #pc state),
        terminated = (#pc state) = (Array.length (#instructions state))
    }

fun execute state =
    let
        val ins = currentInstruction state
        val next = case ins
             of SOME (NOP _)         => inc state
              | SOME (ACC (Plus n))  => inc(acc(n,  state))
              | SOME (ACC (Minus n)) => inc(dec(n, state))
              | SOME (JMP opn)       => jmp(opn, state)
              | NONE                 => state
    in visit(next)
    end


val example = List.mapPartial readInstruction [
        "nop +0", "acc +1", "jmp +4", "acc +3", "jmp -3",
        "acc -99", "acc +1", "jmp -4", "acc +6"]

fun run state condition = if condition state
                          then state
                          else let val newstate = execute state
                               in run newstate condition
                               end

val input = let val stream = TextIO.openIn "./input/8"
                val str = TextIO.inputAll stream
                val _ = TextIO.closeIn stream
                val lines = String.tokens (fn x => x = #"\n") str
            in List.mapPartial readInstruction lines
            end

fun one () =
    let val state = initState input
        val endstate = run state (fn s => #looped s)
    in
        #acc endstate
    end

val example2 = List.mapPartial readInstruction [
        "nop +0", "acc +1", "jmp +4", "acc +3", "jmp -3",
        "acc -99", "acc +1", "nop -4", "acc +6"]

fun flipInstruction i = case i
                         of (NOP n) => JMP n
                          | (JMP n) => NOP n
                          | _ => i

fun flipN (arr, i) =
    let val _ = Array.update(arr, i, flipInstruction (Array.sub(arr, i)))
    in arr
    end

fun mutate (state: State, i) = {
        pc = #pc state,
        acc = #acc state,
        instructions = flipN(#instructions state, i),
        visited = #visited state,
        terminated = #terminated state,
        looped = #looped state
    }

fun findMutation state n = let
                               val candidate = mutate(state, n)
                               val result = run candidate (fn s => #terminated s orelse #looped s)
                           in if #terminated result
                              then SOME result
                              else if n = 0 then NONE
                              else let val _ = mutate(state, n) (* bc mutate is in place *)
                              in findMutation state (n - 1)
                              end
                           end

fun init (inst: Instruction array) = {
        pc = 0,
        acc = 0,
        instructions = inst,
        visited = Intset.empty,
        looped = false,
        terminated = false
    }

fun two () = let val state: State = initState input
                 val endstate = findMutation state ((length input) - 1)                 
             in case endstate
                 of SOME s => #acc s
                 |  _ => 0
             end

fun main () =
    let
        val _ = print "Answer 1: "
        val _ = print (Int.toString(one()))
        val _ = print "\nAnswer 2: "
        val _ = print (Int.toString(two()))
    in print "\n" end

val _ = main ()
