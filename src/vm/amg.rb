
typenames = {:fint => 'OVFixedInt',
             :ufint => 'OVUFixedInt',
             :int => 'OVInt',
             :float => 'OVFloat',
             :string => 'OVString',
             :atom => 'OVAtom'}

modnames = {:fint => 'Int64',
            :ufint => 'Uint64',
            :int => 'Big_int',
            :atom => 'Uint64'}

stdint_ops = {'ADD' => 'add', 'SUB' => 'sub', 'MUL' => 'mul',
              'DIV' => 'div', 'MOD' => 'rem', 'AND' => 'logand',
              'OR' => 'logor', 'XOR' => 'logxor', 'NOT' => 'lognot',
              'SHL' => 'shift_left', 'SHR' => 'shift_right',
              'LSHR' => 'shift_right_logical'}

big_int_ops = {'ADD' => 'add_big_int', 'SUB' => 'sub_big_int',
               'MUL' => 'mult_big_int', 'DIV' => 'div_big_int',
               'MOD' => 'mod_big_int', 'AND' => 'and_big_int',
               'OR' => 'or_big_int', 'XOR' => 'xor_big_int',
               'SHL' => 'shift_left_big_int',
               'SHR' => 'shift_right_big_int',
               'LSHR' => 'shift_right_towards_zero_big_int'}

fint_conv = {:fint => '',
             :ufint => 'Int64.of_uint64',
             :int => 'Int64.of_string @@ Big_int.string_of_big_int',
             :string => 'Int64.of_string',
             :float => 'Int64.of_float',
             :oint => 'Int64.to_int'}

ufint_conv = {:fint => 'Uint64.of_int64',
              :ufint => '',
              :int => 'Uint64.of_string @@ Big_int.string_of_big_int',
              :string => 'Uint64.of_string',
              :float => 'Uint64.of_float',
              :oint => 'Uint64.to_int'}

big_int_conv = {:fint => 'Big_int.big_int_of_string @@ Int64.to_string',
                :ufint => 'Big_int.big_int_of_string @@ Uint64.to_string',
                :int => '', :oint => 'Big_int.int_of_big_int',
                :string => 'Big_int.big_int_of_string'} # no float to big_int function

float_conv = {:fint => 'Int64.to_float',
              :ufint => 'Uint64.to_float',
              :int => 'Big_int.float_of_big_int',
              :string => 'float_of_string',
              :float => ''}

string_conv = {:fint => 'Int64.to_string',
               :ufint => 'Uint64.to_string',
               :int => 'Big_int.string_of_big_int',
               :string => '',
               :float => 'string_of_float'}

gen_conv = lambda do |inst, result_type, conv_hash|
  dts = []

  for k, v in conv_hash
    if k == :oint
      next
    end

    dts.push "#{typenames[k]}(i) -> #{v} i"
  end

  "#{inst} -> (#{typenames[result_type]}(match v with
     #{dts.join("\n    | ")}
   | _ -> failwith \"Invalid type to convert from.\"))"
end

fint_conv_code = gen_conv.call 'TO_FINT', :fint, fint_conv
ufint_conv_code = gen_conv.call 'TO_UFINT', :ufint, ufint_conv
int_conv_code = gen_conv.call 'TO_INT', :int, big_int_conv
float_conv_code = gen_conv.call 'TO_FLOAT', :float, float_conv
string_conv_code = gen_conv.call 'TO_STR', :string, string_conv
convs = [fint_conv_code, ufint_conv_code, int_conv_code,
         float_conv_code, string_conv_code].join("\n    | ")


conv_def = "let conversions inst dss =
  let v = dspop dss
  in dspush dss (match inst with
    #{convs}
  | _ -> failwith \"Not possible.\");;"

float_ops = {'ADD' => '(+.)', 'SUB' => '(-.)', 'MUL' => '( *.)',
             # avoid (*.) to be part of the comments
             'DIV' => '(/.)'}

op_category = {:fint => stdint_ops, :ufint => stdint_ops, :atom => stdint_ops,
               :int => big_int_ops, :float => float_ops}
conv_category = {:fint => fint_conv, :ufint => ufint_conv, :float => float_conv,
                 :string => string_conv, :int => big_int_conv}

gen_binary_inst = lambda do |inst, supported_data_types, atoms|
  dts = []
  for d in supported_data_types
    opname = op_category[d][inst]
    dot = if modnames.include? d
            '.'
          else
            ''
          end
    dts.push "#{typenames[d]}(i), #{typenames[d]}(j) ->
 #{typenames[d]}(#{modnames[d]}#{dot}#{opname} j i)"
  end

  atom = if atoms
           "    | OVAtom(i), OVAtom(j) -> if not ((is_boolean i) || (is_boolean j))
                  then failwith \"Invalid atom value.\"
                  else OVAtom(Uint64.#{op_category[:ufint][inst]} j i)"
         else
           ""
         end

  "#{inst} -> (match v1, v2 with
      #{dts.join("\n    | ")}
#{atom}
    | _ -> failwith \"Incompatible type to do #{inst}\")"
end

stdtypes = [:fint, :ufint, :int, :float]
inttypes = [:fint, :ufint, :int]
add = gen_binary_inst.call 'ADD', stdtypes, false
sub = gen_binary_inst.call 'SUB', stdtypes, false
mul = gen_binary_inst.call 'MUL', stdtypes, false
div = gen_binary_inst.call 'DIV', stdtypes, false
mod = gen_binary_inst.call 'MOD', inttypes, false
and_ = gen_binary_inst.call 'AND', inttypes, true
or_ = gen_binary_inst.call 'OR', inttypes, true
xor = gen_binary_inst.call 'XOR', inttypes, false

binary_def = "let binary_arithmetics inst dss =
  let v1 = dspop dss
  in let v2 = dspop dss
  in dspush dss (match inst with
    #{[add, sub, mul, div, mod, and_, or_, xor].join("\n  | ")}
  | _ -> failwith \"Not possible.\");;
"

gen_shift = lambda do |inst, supported_data_types|
  dts = []
  for d in supported_data_types
    opname = op_category[d][inst]
    dot = if modnames.include? d
            '.'
          else
            ''
          end
    dts.push "OVUFixedInt(i), #{typenames[d]}(j) ->
      let ii = Uint64.to_int i in #{typenames[d]}(#{modnames[d]}#{dot}#{opname} j ii)"
  end

  "#{inst} -> (match v1, v2 with
      #{dts.join("\n    | ")}
    | _ -> failwith \"Incompatible type to do #{inst}\")"
end

shl = gen_shift.call 'SHL', inttypes
shr = gen_shift.call 'SHR', inttypes
lshr = gen_shift.call 'LSHR', inttypes

shift_def = "let shift_arithmetics inst dss =
  let v1 = dspop dss
  in let v2 = dspop dss
  in dspush dss (match inst with
    #{[shl, shr, lshr].join("\n | ")}
  | _ -> failwith \"Not possible.\");;
"

gen_unary_instruction = lambda do |inst, supported_data_types, lognot|
  dts = []
  for d in supported_data_types
    if d != :int
      opname = op_category[d][inst]
      dts.push "#{typenames[d]}(i) -> #{typenames[d]}(#{modnames[d]}.#{opname} i)"
    else
      dts.push "OVInt(i) -> OVInt(Big_int.sub_big_int
              (Big_int.minus_big_int i) big_minus_one)"
    end
  end
  if lognot
    dts.push "OVAtom(i) -> if i = Uint64.one then OVAtom(Uint64.zero)
                     else if i = Uint64.zero then OVAtom(Uint64.one)
                     else failwith \"Invalid atom value.\""
  end

  "#{inst} -> (match v with
      #{dts.join("\n    | ")}
    | _ -> failwith \"Incompatible type to do #{inst}\")"
end

not_ = gen_unary_instruction.call 'NOT', inttypes, true

unary_def = "let unary_arithmetics inst dss =
  let v = dspop dss
  in dspush dss (match inst with
    #{[not_].join("\n | ")}
  | _ -> failwith \"Not possible.\");;
"

open "#{ARGV[0]}/arithmetics.ml", 'w' do |f|
  f.write "open Stdint;;
open T;;
open Nstack;;
open Tasm_ast;;

let is_boolean a = if a = Uint64.one || a = Uint64.zero
  then true
  else false;;

let big_minus_one = Big_int.big_int_of_int (-1);;

"

  f.write conv_def
  f.write binary_def
  f.write shift_def
  f.write unary_def
end
