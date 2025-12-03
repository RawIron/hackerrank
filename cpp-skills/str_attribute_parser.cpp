#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <regex>

using namespace std;


namespace Parse {
    
#define DEBUG false

typedef int StatusCode;

namespace {
    constexpr StatusCode Ok{ 0 };
    constexpr StatusCode NoStartTagFound{ -10 };
    constexpr StatusCode MissingClose{ -11 };
    constexpr StatusCode MissingEnd{ -12 };
};

typedef string Scope;
typedef map<string, string> SymbolTable;

typedef struct {
    Scope block;
    string::const_iterator pos_code;
    const string& code;
    SymbolTable symbols;
} ParseState;


/**
 *  match open tag
 *  append the tagname to the scope
 *  advance the scanning position past the match
 */
StatusCode match_start(ParseState& parse_state) {
    auto& [block, pos_code, code, _] = parse_state;

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
    pos_code = match.suffix().first;
    
#if DEBUG
    for (size_t i{0}; i < match.size(); ++i) {
        cout << match[i] << endl;
    }
    cout << block << " " << distance(code.cbegin(), pos_code);
#endif

    return Ok;
}

/**
 * match does not contain all capture groups!
 * input:   another="another" final="final"
 * output:  another="another" final="final"
             final="final"
            final
            final
 */
StatusCode match_attributes_fail(ParseState& parse_state) {
    auto& [block, pos_code, code, symbols] = parse_state;

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


/**
 * determine position of tag close
 * use that position as the end in matching
 * match all attributes
 * append (key, value) to SymbolTable
 * advance the scanning position past the match
 */
StatusCode match_attributes_token(ParseState& parse_state) {
    auto& [block, pos_code, code, symbols] = parse_state;
    sregex_iterator stop;

    const regex close_tag{ R"(\s*>)" };  // (>) or (   >)
    sregex_iterator close_itr{ pos_code, code.cend(), close_tag };     
    if (close_itr == stop) { return MissingClose; }

    const smatch& match_close{ *close_itr };
    string::const_iterator pos_close{ match_close.suffix().first };

    // ((name) =  ("value"))
    const regex attribute{ R"(\s*(\w+)\s*=\s*\"(\w+)\")" };
    sregex_token_iterator token(pos_code, pos_close-1,
                                attribute, {1, 2}); // 1 = name, 2 = value
    sregex_token_iterator tok_stop;

    for (; token != tok_stop; ++token) {
        std::cout << *token << endl;
    }
    // advance pos_code ??
    
    return Ok;
}


StatusCode match_attributes_itr(ParseState& parse_state) {
    auto& [block, pos_code, code, symbols] = parse_state;

    sregex_iterator stop;

    const regex close_tag{ R"(\s*>)" };  // (>) || (   >)
    sregex_iterator close_itr{ pos_code, code.cend(), close_tag };     
    if (close_itr == stop) { return MissingClose; }

    const smatch& match_close{ *close_itr };
    string::const_iterator pos_close{ match_close.suffix().first };

    // ((name)  =  ("value"))
    const regex attribute{ R"(\s*(\w+)\s*=\s*\"([a-zA-Z0-9_\-%$#@!?.]+)\")" };  
    sregex_iterator match_itr{ pos_code, pos_close-1, attribute };

    while (match_itr != stop) {
        const smatch& match{ *match_itr };
        string symbol{};
        symbol.append(block).append("~").append(match[1]);
        symbols[symbol] = match[2];
        pos_code = match.suffix().first;

#if DEBUG
        for (auto sub : match) {
            cout << sub << endl;
        }
#endif
        ++match_itr;
    }
#if DEBUG
    cout << distance(code.cbegin(), pos_code);
#endif

    return Ok;
}


/**
 * match close tag
 * advance the scanning position past the match
 */
StatusCode match_close(ParseState& parse_state) {
    auto& [block, pos_code, code, _] = parse_state;

    sregex_iterator stop;
    const regex close_tag{ R"(\s*>)" };  // (>) || (   >)
    sregex_iterator close_itr{ pos_code, code.cend(), close_tag };     
    if (close_itr == stop) { return MissingClose; }

    const smatch& match_close{ *close_itr };
    pos_code = match_close.suffix().first;
    
#if DEBUG
    for (auto sub : match_close) {
        cout << sub << endl;
    }
    cout << distance(code.cbegin(), pos_code);
#endif
    
    return Ok;
}


/**
 * match end tag
 * remove the tagname from the scope
 * advance the scanning position past the match
 */
StatusCode match_end(ParseState& parse_state) {
    auto& [block, pos_code, code, _] = parse_state;

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

#if DEBUG
    for (auto sub : match_end) {
        cout << sub << endl;
    }
    cout << block << " " << distance(code.cbegin(), pos_code) << endl;
#endif

    
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
    auto& [_, pos_code, code, __] = parse_state;
    sregex_iterator end_itr{};
    smatch match{};
    sregex_iterator nested_itr{};
    sregex_iterator stop;

    while (true) {    
        const regex end_tag{ R"(</(\w+)>)" };  // (</tagname>)
        end_itr = { pos_code, code.cend(), end_tag };     
        if (end_itr == stop) { return MissingEnd; }
        
        match = *end_itr;
        string::const_iterator pos_end = match.prefix().second;

        const regex start_tag{ R"(<(\w+))" };  // (<(tagname))
        nested_itr = { pos_code, pos_end, start_tag };
        if (nested_itr != stop) {
            status = parse_tag( parse_state );
            if (status != Ok) { return status; }
        }
        else { break; }
    }

    // end tag
    match_end( parse_state );
    if (status != 0) { return status; }

    return Ok;
}


map<string,string> parse(const string& code) {
    string::const_iterator pos{ code.cbegin() };
    ParseState state{ {}, pos, code, {} };

    while (state.pos_code != code.cend()) {
        StatusCode status{ parse_tag( state ) };
        if (status != Ok) {
            break;
        }
    }
    
#if DEBUG
    cout << "Symbols" << endl;
    for (auto [symbol, value] : state.symbols) {
        cout << symbol << " " << value << endl;
    }
#endif

    return state.symbols;
}


#include <cassert>
void run_tests() {
    string code{};
    map<string,string> symbols{};

    // no input
    code = "";
    assert( parse(code).empty() );

    // no attributes
    code ="<tag1> <tag2> </tag2> </tag1>";
    assert( parse(code).empty() );

    // one or more than one attribute
    // list nesting
    //    o--
    code = "<tag1 value = \"value\"   final=\"final\"> <tag2 name = \"name\"> <tag3 another=\"another\" final=\"final\"> </tag3> </tag2> </tag1>";
    //      ^   ^                                    ^     ^                ^     ^                                    ^       ^       ^       ^
    //      0   4                                    37    43               58    64                                   97      105     113      121
    symbols = parse(code);
    assert( symbols.size() == 5 );
    assert( symbols["tag1.tag2.tag3~final"] == "final" );

    // one or more than one attribute
    // tree nesting
    //    o
    //   | \
    //      \
    //       \
    
    code = "<a value = \"GoodVal\"> <b value = \"BadVal\" size = \"10\"> </b> <c height = \"auto\"> <d size = \"3\"> <e strength = \"2\"> </e> </d> </c> </a>";
    symbols = parse(code);
    assert( symbols.size() == 6 );
    assert( symbols["a.c~height"] == "auto" );
    assert( symbols["a.b~value"] == "BadVal" );

    // one or more than one attribute
    // list nesting
    //    o------
    // value can contain characters like '%$!?.'
    code = "<a value = \"Good!\"> <b value = \"Bad$\" size = \"10.34\"> <c height = \"auto?\"> <d size = \"3\"> <e strength = \"200%\"> <f a1 = \"1.0\" a2 = \"2\" a3 = \"3\"> </f> </e> </d> </c> </b> </a>";
    symbols = parse(code);
    assert( symbols.size() == 9 );
    assert( symbols["a.b~value"] == "Bad$" );
    assert( symbols["a.b.c.d.e.f~a1"] == "1.0" );
    assert( symbols["a.b.c.d.e~strength"] == "200%" );
    
    // one or more than one attribute
    // tree nesting
    //   o -  o  - o - o
    //   |   /\    |   |
    code = "<tag1 v1 = \"123\" v2 = \"43.4\" v3 = \"hello\"> </tag1> <tag2 v4 = \"v2\" name = \"Tag2\"> <tag3 v1 = \"Hello\" v2 = \"World!\"> </tag3> <tag4 v1 = \"Hello\" v2 = \"Universe!\"> </tag4> </tag2> <tag5> <tag7 new_val = \"New\"> </tag7> </tag5> <tag6> <tag8 intval = \"34\" floatval = \"9.845\"> </tag8> </tag6>";
    symbols = parse(code);
    assert( symbols.size() == 12 );
    assert( symbols["tag5.tag7~new_val"] == "New" );
}

}


void answer(map<string,string> symbols, vector<string> queries) {
    for (auto query : queries) {
        // C++20
        // if (symbols.contains(query)) {
        const auto it{ symbols.find(query) };
        if (it != symbols.end()) {
            cout << it->second << endl;
        }
        else {
            cout << "Not Found!" << endl;
        }
    }
    return;
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

    map<string,string> symbols{ Parse::parse(code) };
    answer(symbols, queries); 

    //Parse::run_tests();

    return EXIT_SUCCESS;
}
