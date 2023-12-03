#%%
import numpy as np
import re

with open('../../resources/day03.txt', 'r') as f:
    data = f.read()
    input = data.splitlines()

testdata = \
"""467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
testinput = testdata.splitlines()
#%%
is_digit = re.compile('\d')
symbols = {x for x in data if (x != '.') and (not is_digit.match(x)) and (x != '\n')}

#%%
def is_partnumber(input, start, end, lineidx):
    def check_line(idx):
        l = max(start-1, 0)
        r = min(end+1, len(input[idx])-1)
        return any([x in symbols for x in input[idx][l:r]])
    
    above = check_line(lineidx-1) if lineidx > 0 else False
    below = check_line(lineidx+1) if lineidx < len(input)-1 else False
    lr = check_line(lineidx)
    return any((above, below, lr))

#%%
numbers = re.compile(r'\d+')

def doit(input):
    for i in range(len(input)):
        for m in numbers.finditer(input[i]):
            if is_partnumber(input, m.start(), m.end(), i):
                yield int(m.group(0))

print(f"Sum is {sum(doit(input))}")

#%%
def find_adjacent_numbers(input, i, j):
    firstline = max(i-1, 0)
    lastline = min(i+1, len(input)-1)
    for idx in range(firstline, lastline+1):
        for m in numbers.finditer(input[idx]):
            if (j >= m.start() - 1) and (j < m.end() + 1):
                yield int(m.group(0))

# %%
def find_gears(input):
    starfinder = re.compile(r'\*')
    for i, line in enumerate(input):
        for m in starfinder.finditer(line):
            adjacent = [*find_adjacent_numbers(input, i, m.start())]
            if len(adjacent) == 2:
                yield adjacent

for x in find_gears(testinput):
    print(x)

print(f"Part 2 sum is: {sum([x[0]*x[1] for x in find_gears(input)])}")
# %%
