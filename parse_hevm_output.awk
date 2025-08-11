#!/usr/bin/awk -f

{
    if ($0 ~ /^"Return: 0x/) {
        hex = substr($0, 10, length($0) - 10)
        decimal = strtonum(hex)
        print "Return:", decimal
	print "hex:", hex
    }
    else if ($0 ~ /^"Revert: 0x/) {
        hex = substr($0, 10, length($0) - 10)
        decimal = strtonum(hex)
        print "Revert:", decimal
	print "hex:", hex
	}
    else {
        print
    }
}
