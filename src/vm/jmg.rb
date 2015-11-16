
prelude = "open Tasm_ast;;
open Ort;;
open T;;
open Stdint;;
open Vm_t;;

let branch tods mods next_ip =
  let m = Hashtbl.find mods (snd tods)
  in function"

assemble_big = lambda {|x| "Big_int.#{x}_big_int i Big_int.zero_big_int"}

match = 'let _ip = match (lookup_val m.ort tods).v with'
clause = 'then j else next_ip'

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
  ['JT', 'OVAtom', 'Uint64.compare i Uint64.one = 0'],
  ['JF', 'OVAtom', 'Uint64.compare i Uint64.zero = 0'],
].map do |xs|
  jn, cons, cond = xs

  "| #{jn}(ArgLit(VUFixedInt(j)))
| H#{jn}(ArgLit(VUFixedInt(j))) ->
  #{match}
      #{cons}(i) -> if #{cond}
      #{clause}
    | _ -> failwith \"Unsupported data type.\"
  in _ip"
end
nncs = nonnumerical_conditionals.join "\n\n"

e_and_ne_conditionals = [
  ['JE',
   ['OVTNil', 'OVLNil'],
   ['OVTuple(_)', 'OVList(_)']],
  ['JNE',
   ['OVTuple(_)', 'OVList(_)'],
   ['OVTNil', 'OVLNil']]
].map do |xs|
  jn, ts, fs = xs

  "| #{jn}(ArgLit(VUFixedInt(j)))
| H#{jn}(ArgLit(VUFixedInt(j))) ->
  #{match}
      #{ts[0]}
    | #{ts[1]} -> j
    | #{fs[0]}
    | #{fs[1]} -> next_ip
    | _ -> failwith \"Unsupported data type.\"
  in _ip"
end
eancs = e_and_ne_conditionals.join "\n\n"

conditionals = [ncs, nncs, eancs].join "\n\n"

File.open "#{ARGV[0]}/jumps.ml", 'w' do |f|
  f.write prelude
  f.write "\n"
  f.write conditionals
  f.write "\n"
  f.write "| _ -> failwith \"Not possible.\"
"
end

