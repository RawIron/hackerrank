#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <regex>

using namespace std;


typedef int ParseErrorCode;

namespace {
    constexpr ParseErrorCode Ok{ 0 };
    constexpr ParseErrorCode NoStartTagFound{ -10 };
    constexpr ParseErrorCode MissingClose{ -11 };
    constexpr ParseErrorCode MissingEnd{ -12 };
};

typedef string Block;

typedef struct {
    ParseErrorCode status;
    Block block;
    string::const_iterator pos_code;
} ParseStatus;

typedef struct {
    Block block;
    string::const_iterator pos_code;
    const string& code;
} ParseState;

typedef map<Block, string> SymbolTable;


/**
 *
 */
ParseStatus match_start(ParseState parse_state) {
    auto [block, pos_code, code] = parse_state;

    const regex start_tag{ R"(<(\w+))" };  // (<(tagname))

    sregex_iterator match_itr{ pos_code, code.cend(), start_tag };
    sregex_iterator stop;
    if (match_itr == stop) { return { NoStartTagFound, block, pos_code }; }

    const smatch& match{ *match_itr };
    const string tag_name{ match[1] };
    if (block.empty()) {
        block = tag_name;
    }
    else {
        block += "." + tag_name;
    }
    
    for (size_t i{0}; i < match.size(); ++i) {
        cout << match[i] << endl;
    }
    pos_code = match.suffix().first;  // advance the scanning position past the match

    return { Ok, block, pos_code };
}


/**
 * match for regex does not contain all capture groups!
 * input:   another="another" final="final"
 * output:  another="another" final="final"
             final="final"
            final
            final
 */
ParseStatus match_attributes_fail(ParseState parse_state) {
    auto [block, pos_code, code] = parse_state;

    // (name  =  "value") (key = "number") ..
    const regex attribute{ R"((\s*(\w+)\s*=\s*\"(\w+)\")+)" };    

    sregex_iterator match_itr{ pos_code, code.cend(), attribute };
    sregex_iterator stop;

    if (match_itr != stop) {
        const smatch& match{ *match_itr };
        for (auto sub : match) {
            cout << sub << endl;
        }
        pos_code = match.suffix().first;
    }
    return {Ok, block, pos_code};
}



ParseStatus match_attributes_token(ParseState parse_state) {
    auto [block, pos_code, code] = parse_state;
    sregex_iterator stop;

    const regex close_tag{ R"(\s*>)" };  // (>) or (   >)
    sregex_iterator close_itr{ pos_code, code.cend(), close_tag };     
    if (close_itr == stop) { return { MissingClose, block, pos_code }; }

    const smatch& match_close{ *close_itr };
    string::const_iterator pos_close{ match_close.suffix().first };

    // (name  =  "value")
    const regex attribute{ R"(\s*(\w+)\s*=\s*\"(\w+)\")" };
    sregex_token_iterator token(pos_code, pos_close-1,
                                attribute, {1, 2}); // 1 = name, 2 = value
    sregex_token_iterator tok_end;

    for (; token != tok_end; ++token) {
        std::cout << *token << endl;
    }
    // advance pos_code ??
    
    return {Ok, block, pos_code};
}


ParseStatus match_attributes_itr(ParseState parse_state) {
    auto [block, pos_code, code] = parse_state;

    sregex_iterator stop;

    const regex close_tag{ R"(\s*>)" };  // (>) || (   >)
    sregex_iterator close_itr{ pos_code, code.cend(), close_tag };     
    if (close_itr == stop) { return { MissingClose, block, pos_code }; }

    const smatch& match_close{ *close_itr };
    for (auto sub : match_close) {
        cout << sub << endl;
    }
    string::const_iterator pos_close{ match_close.suffix().first };

    // (name  =  "value")
    const regex attribute{ R"(\s*(\w+)\s*=\s*\"(\w+)\")" };  
    sregex_iterator match_itr{ pos_code, pos_close-1, attribute };

    while (match_itr != stop) {
        const smatch& match{ *match_itr };
        for (auto sub : match) {
            cout << sub << endl;
        }
        ++match_itr;
        pos_code = match.suffix().first;
    }
    
    return {Ok, block, pos_code};
}


/**
 *  parse nested tags like
 *      <tag1 name = "name1"> <tag2 key = "value" word = "token"> </tag2> </tag1>
 *
 */
ParseStatus parse_tag(ParseState parse_state) {
    auto [block, pos_code, code] = parse_state;

    sregex_iterator stop;    

    // start tag
    const regex start_tag{ R"(<(\w+))" };  // (<tagname)
    sregex_iterator start_itr{ pos_code, code.cend(), start_tag };
    if (start_itr == stop) { return { NoStartTagFound, block, pos_code }; }

    const smatch& match{ *start_itr };
    const string tag_name{ match[1] };
    if (block.empty()) {
        block = tag_name;
    }
    else {
        block += "." + tag_name;
    }
    pos_code = match.suffix().first;  // advance the scanning position past the match
    
    for (size_t i{0}; i < match.size(); ++i) {
        cout << match[i] << endl;
    }
    cout << block << " " << distance(code.cbegin(), pos_code);
    
    // optional attributes
    auto [status, blck, pos] = match_attributes_itr( {block, pos_code, code} );
    if (status != 0) { return {status, blck, pos}; }
    pos_code = pos;
    block = blck;
    cout << distance(code.cbegin(), pos_code);
    
    // close tag
    const regex close_tag{ R"(\s*>)" };  // (>) || (   >)
    sregex_iterator close_itr{ pos_code, code.cend(), close_tag };     
    if (close_itr == stop) { return { MissingClose, block, pos_code }; }

    const smatch& match_close{ *close_itr };
    pos_code = match_close.suffix().first;
    
    for (auto sub : match_close) {
        cout << sub << endl;
    }
    cout << distance(code.cbegin(), pos_code);
    
    // nested start tag
    sregex_iterator nested_itr{ pos_code, code.cend(), start_tag };
    if (nested_itr != stop) {
        auto [status, blck, pos] = parse_tag( {block, pos_code, code} );
        if (status != 0) {
            return {status, blck, pos};
        }
        pos_code = pos;
        block = blck;
    }
    cout << distance(code.cbegin(), pos_code);
    
    // end tag
    const regex end_tag{ R"(</(\w+)>)" };  // (</tagname>)
    sregex_iterator end_itr{ pos_code, code.cend(), end_tag };     
    if (end_itr == stop) { return {MissingEnd, block, pos_code}; }

    const smatch& match_end{ *end_itr };
    pos_code = match_end.suffix().first;

    size_t pos_dot = block.rfind('.');
    if (pos_dot == string::npos) {
        block = "";
    }
    else {
        block = block.substr(0, pos_dot);     
    }


    for (auto sub : match_end) {
        cout << sub << endl;
    }
    cout << block << " " << distance(code.cbegin(), pos_code);
    
    return {Ok, block, pos_code};  
}


void parse(const string& code) {
    string::const_iterator pos{ code.cbegin() };

    while (pos != code.cend()) {
        auto [status, block, pos_code] = parse_tag( {{}, pos, code} );
        if (status != Ok) {
            break;
        }
        pos = pos_code;
    }
}


void answer(vector<string> queries) {
  
}


#include <cassert>
void run_test() {
    string code{};
    code = "<tag1 value = \"value\"   final=\"final\"> <tag2 name = \"name\"> <tag3 another=\"another\" final=\"final\"> </tag3> </tag2> </tag1>";
    //      ^   ^                                    ^     ^                ^     ^                                    ^       ^       ^       ^
    //      0   4                                    37    43               58    64                                   97      105     113      121
     
}


#include <limits>
string read_source(const int lines_total) {
    string text{};
    for (int i{0}; i<lines_total; ++i) {
      string line{};
      getline(cin, line);
      text += line;
    }
    return text;
}

vector<string> read_queries(const int queries_total) {
    vector<string> queries{};
    for (int i{0}; i<queries_total; ++i) {
      string line{};
      getline(cin, line);
      queries.push_back(line);
    }
    return queries;  
}


int main() {
    int lines_total{0};
    int queries_total{0};
    cin >> lines_total >> queries_total;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
    
    const string code{ read_source(lines_total) };
    vector<string> queries{ read_queries(queries_total)};
    
    parse(code);
    answer(queries);
    
    return EXIT_SUCCESS;
}
