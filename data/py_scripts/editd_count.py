# script to calculate the number of unique strings within a certain edit distance of
# an input string

import sys

alphabet = 'abcdefghijklmnopqrstuvwxyz'

def edits(word):
   splits     = [(word[:i], word[i:]) for i in range(len(word) + 1)]
   deletes    = [a + b[1:] for a, b in splits if b]
   replaces   = [a + c + b[1:] for a, b in splits for c in alphabet if b]
   inserts    = [a + c + b     for a, b in splits for c in alphabet]
   return set(deletes + replaces + inserts)

word = sys.argv[1]
edit_d = int(sys.argv[2])
matches = set(word)
while edit_d > 0:
    matches.update([x for l in matches for x in edits(l)])
    edit_d -= 1

print len(matches)


