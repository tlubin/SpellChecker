from __future__ import print_function
import re

input_ptr = open('dict/dict_500k', 'rb')
output_ptr = open('dict/dict.txt', 'wb')

for line in input_ptr:
	if re.match("^[A-Za-z]*$", line):
		output_ptr.write(line)

input_ptr.close()
output_ptr.close()