
prelude = "open Tasm_ast;;
open T;;
open Stdint;;
open Vm_t;;

let branch v next_ip =
  function"

assemble_big = lambda {|x| "Big_int.#{x}_big_int i Big_int.zero_big_int"}

match = 'let j_ = Uint64.to_int j in let _ip = match v with'
clause = 'then j_ else next_ip'

numerical_conditionals = [
  ['JNEZ', '<>',
   lambda {'not (Big_int.eq_big_int i Big_int.zero_big_int)'}],

  ['JEZ', '=',
   lambda {assemble_big.call 'eq'}],

  ['JGZ', '>',
   lambda {assemble_big.call 'gt'}],

  ['JGEZ', '>=',
   lambda {assemble_big.call 'ge'}],

  ['JLZ', '<',
   lambda {assemble_big.call 'lt'}],

  ['JLEZ', '<=',
   lambda {assemble_big.call 'le'}],
].map do |xs|
  jn, nb_cond, b_cond = xs

  "| #{jn}(ArgLit(VUFixedInt(j)))
| H#{jn}(ArgLit(VUFixedInt(j))) ->
  #{match}
      OVInt(i) -> if #{b_cond.call}
      #{clause}
    | OVFixedInt(i) -> if Int64.compare i Int64.zero #{nb_cond} 0
      #{clause}
    | OVUFixedInt(i) -> if Uint64.compare i Uint64.zero #{nb_cond} 0
      #{clause}
    | OVFloat(i) -> if Pervasives.compare i 0.0 #{nb_cond} 0
      #{clause}
    | _ -> failwith \"Unsupported data type.\"
  in _ip"
end
ncs = numerical_conditionals.join "\n\n"

nonnumerical_conditionals = [
  ['JT', 'OVAtom', 'Uint64.zero'],
  ['JF', 'OVAtom', 'Uint64.one'],
].map do |xs|
  jn, cons, cond = xs

  "| #{jn}(ArgLit(VUFixedInt(j)))
| H#{jn}(ArgLit(VUFixedInt(j))) ->
  #{match}
      #{cons}(i) -> if Uint64.compare i #{cond} = 0
      (* Take JF as an example,
         only fall through when i = true, i.e. only jump when i <> true. *)
      then next_ip else j_
    | _ -> j_
  in _ip"
end
nncs = nonnumerical_conditionals.join "\n\n"

conditionals = [ncs, nncs].join "\n\n"

File.open "#{ARGV[0]}/jumps.ml", 'w' do |f|
  f.write prelude
  f.write "\n"
  f.write conditionals
  f.write "\n"
  f.write "| _ -> failwith \"Not possible.\"
"
end

