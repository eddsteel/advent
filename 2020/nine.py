class Window:

    def __init__(self, preamble, size):
        self.data = preamble
        self.win = slice(-size, None)

    def window(self):
        return self.data[self.win]

    def push(self, i):
        self.data.append(i)
        return self.window()


def sum_matches(data, target):
    return [(data[a], data[b])
            for a in range(len(data))
            for b in range(a + 1, len(data))
            if data[a] + data[b] == target]

def accept(window, i):
    if len(sum_matches(window.window(), i)):
        window.push(i)
        return True
    else:
        False

def examples():
    example = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95,
            102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
    print(star_one(example, 5))
    print(star_two(example, star_one(example, 5)))

def star_one(data, n):
    win = Window(data[:n], n)
    searching = True
    counter = n
    while searching:
        searching = accept(win, data[counter])
        counter += 1

    return data[counter - 1]

def sliding_windows(data):
    return [
        data[x:x + size]
        for size in range(2,len(data) - 1)
        for x in range(len(data) - size)]

def star_two(data, target):
    result = next(x for x in sliding_windows(data) if sum(x) == target)
    return min(result) + max(result)

input_data = [int(n) for n in open("./input/9").readlines()]
one = star_one(input_data, 25)

print("Answer 1:")
print(one)
print("Answer 2:")
print(star_two(input_data, one))
