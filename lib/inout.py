
def parse_input():
    _ = int(input().strip())
    return map(int, input().rstrip().split())


def show(result):
    fileno = os.environ.get('OUTPUT_PATH', sys.stdout.fileno())
    with open(fileno, 'w') as out:
        out.write(str(result) + '\n')