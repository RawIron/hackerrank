def calculate_score(word):
    VOWELS = ['A', 'E', 'I', 'O', 'U']

    vowels = consonants = 0
    for ix, letter in enumerate(reversed(word)):
        if letter in VOWELS:
            vowels += ix + 1
        else:
            consonants += ix + 1

    return vowels, consonants


def minion_game(word):
    kevin, stuart = calculate_score(word)
    if kevin < stuart:
        print("Stuart %d" % stuart)
    elif kevin > stuart:
        print("Kevin %d" % kevin)
    else:
        print("Draw")

if __name__ == '__main__':
    s = input()
    minion_game(s)
