print "Hello, bare metal Forths!"

define ddd
disas/r $eip,+30
end

set architecture i8086
set disassembly-flavor intel
set tdesc filename target.xml

tar rem :1234
break *0x7c00
break *0x7e00

