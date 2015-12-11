require 'fileutils'

def lang fn
  if fn =~ /rb$/
    'ruby'
  elsif fn =~ /ml.?$/
    'ocaml'
  elsif fn =~ /^wscript$/
    'python'
  elsif fn =~ /\.t$/
    'towel'
  elsif fn =~ /\.p$/
    'ocaml'
  else
    ''
  end
end

def put_src fp, path
  git_ignore = Regexp.new(Regexp.escape(open("../.gitignore").read))

  Dir.foreach path do |entry|
    if entry =~ git_ignore
      next
    end

    if entry =~ /^tasm.*\.ml.?$/
      next
    end
    if ['.travis.yml', 'jumps.ml', 'tokens.ml',
        'typer.ml', 'built_in.ml', 'sscoping.ml'].include? entry
      next
    end
    if entry =~ /bytecode/
      next
    end

    l = lang entry

    if l == ''
      next
    end

    open (File.join path, entry) do |x|
      puts "opened #{path}/#{entry}"

      fp.write "\\begin{mdframed}[style=cl]"
      fp.write "\n"
      if l == 'towel'
        fp.write "\\begin{verbatim}"
      else
        fp.write "\\begin{minted}{#{l}}"
      end
      fp.write "\n"

      if l == 'ruby' || l == 'python'
        fp.write "# #{entry} -- Author: Zihang Chen (zc2324)"
      elsif l == 'ocaml'
        fp.write "(* #{entry} -- Author: Zihang Chen (zc2324) *)"
      elsif l == 'towel'
        fp.write "\"#{entry} -- Author: Zihang Chen (zc2324)\""
      end
      fp.write "\n"

      fp.write x.read
      if l == 'towel'
        fp.write "\\end{verbatim}"
      else
        fp.write "\\end{minted}"
      end
      fp.write "\n"
      fp.write "\\end{mdframed}"
    end
  end
end

open 'xxx.tex', 'w' do |f|
  put_src f, '../src/compiler'
  put_src f, '../src/vm'
  put_src f, '../src/tasm'
  put_src f, '../src/towelibs'
  put_src f, '..'
end
