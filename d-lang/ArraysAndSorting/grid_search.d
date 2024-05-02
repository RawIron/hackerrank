import std.stdio : writeln, readln;
import std.cstream : din;
import std.format : formattedRead;
import std.algorithm, std.array;
import std.string : chomp, indexOf;
import std.conv : to;


string[] read_matrix()
{
    string line = chomp(readln());
    int r, c;
    formattedRead(line, "%s %s", &r, &c);
    
    string[] rows = new string[r];
    foreach (int i ; 0..r) {
        rows[i] = din.readLine().to!string;
    }
    
    return rows;
}

bool search(string[] matrix, string[] pattern) {
    string initial = pattern[0];
    bool match = false;
    
    foreach (i, row; matrix[0..$-pattern.length+1]) {
        long start = 0;
        long pos = indexOf(row, initial, start);
        while (pos >= 0) {
            match = true;
            foreach (j, pat; pattern[1..$]) {
                if (indexOf(matrix[i+j+1], pat) != pos) {
                    match = false;
                    break;
                }
            }
            
            if (match == true) {
                break;
            }
            
            start = pos + 1;
            pos = indexOf(row, initial, start);
        }
        
        if (match == true) {
            break;
        }
    }
    
    return match;
}


int main()
{
    int tests = to!int(din.readLine());
    
    foreach(_ ; 0..tests) {
        string[] matrix = read_matrix();
        string[] pattern = read_matrix();
        
        bool match = search(matrix, pattern);
        
        if (match == true) {
            writeln("YES");
        } else {
            writeln("NO");
        }
    }
    
    return 0;
}
