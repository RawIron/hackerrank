#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <regex>

using namespace std;


namespace Parse {

typedef int StatusCode;

namespace {
    constexpr StatusCode Ok{ 0 };
    constexpr StatusCode NoStartTagFound{ -10 };
    constexpr StatusCode MissingClose{ -11 };
    constexpr StatusCode MissingEnd{ -12 };
};

typedef struct {
    string block;
    string::const_iterator pos_code;
    const string& code;
} ParseState;

typedef map<string, string> SymbolTable;


/**
 *
 */
StatusCode match_start(ParseState& parse_state) {
    auto& [block, pos_code, code] = parse_state;

    const regex start_tag{ R"(<(\w+))" };  // (<(tagname))

    sregex_iterator match_itr{ pos_code, code.cend(), start_tag };
    sregex_iterator stop;
    if (match_itr == stop) { return NoStartTagFound; }

    const smatch& match{ *match_itr };
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

    return Ok;
}

/**
 * match for regex does not contain all capture groups!
 * input:   another="another" final="final"
 * output:  another="another" final="final"
             final="final"
            final
            final
 */
StatusCode match_attributes_fail(ParseState& parse_state) {
    auto& [block, pos_code, code] = parse_state;

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
    return Ok;
}



StatusCode match_attributes_token(ParseState& parse_state) {
    auto& [block, pos_code, code] = parse_state;
    sregex_iterator stop;

    const regex close_tag{ R"(\s*>)" };  // (>) or (   >)
    sregex_iterator close_itr{ pos_code, code.cend(), close_tag };     
    if (close_itr == stop) { return MissingClose; }

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
    
    return Ok;
}


StatusCode match_attributes_itr(ParseState& parse_state) {
    auto& [block, pos_code, code] = parse_state;

    sregex_iterator stop;

    const regex close_tag{ R"(\s*>)" };  // (>) || (   >)
    sregex_iterator close_itr{ pos_code, code.cend(), close_tag };     
    if (close_itr == stop) { return MissingClose; }

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
    cout << distance(code.cbegin(), pos_code);

    return Ok;
}

StatusCode match_close(ParseState& parse_state) {
    auto& [block, pos_code, code] = parse_state;

    sregex_iterator stop;
    const regex close_tag{ R"(\s*>)" };  // (>) || (   >)
    sregex_iterator close_itr{ pos_code, code.cend(), close_tag };     
    if (close_itr == stop) { return MissingClose; }

    const smatch& match_close{ *close_itr };
    pos_code = match_close.suffix().first;
    
    for (auto sub : match_close) {
        cout << sub << endl;
    }
    cout << distance(code.cbegin(), pos_code);
    
    return Ok;
}


StatusCode match_end(ParseState& parse_state) {
    auto& [block, pos_code, code] = parse_state;

    sregex_iterator stop;
    const regex end_tag{ R"(</(\w+)>)" };  // (</tagname>)
    sregex_iterator end_itr{ pos_code, code.cend(), end_tag };     
    if (end_itr == stop) { return MissingEnd; }

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
    
    return Ok;   
}


/**
 *  parse nested tags like
 *      <tag1 name = "name1"> <tag2 key = "value" word = "token"> </tag2> </tag1>
 *
 */
StatusCode parse_tag(ParseState& parse_state) {
    StatusCode status{};
    
    // start tag
    status = match_start( parse_state );
    if (status != 0) { return status; }
    
    // optional attributes
    status = match_attributes_itr( parse_state );
    if (status != 0) { return status; }
    
    // close tag
    status = match_close( parse_state );
    if (status != 0) { return status; }
    
    // nested start tag
    sregex_iterator stop;
    const regex start_tag{ R"(<(\w+))" };  // (<(tagname))
    sregex_iterator nested_itr{ parse_state.pos_code, parse_state.code.cend(), start_tag };
    if (nested_itr != stop) {
        status = parse_tag( parse_state );
        if (status != 0) { return status; }
    }
    
    // end tag
    match_end( parse_state );
    if (status != 0) { return status; }

    return Ok;
}


void parse(const string& code) {
    string::const_iterator pos{ code.cbegin() };
    ParseState state{ {}, pos, code };
    StatusCode status{};
    
    while (state.pos_code != code.cend()) {
        status = parse_tag( state );
        if (status != Ok) {
            break;
        }
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
    
    Parse::parse(code);
    Parse::answer(queries);
    
    return EXIT_SUCCESS;
}
