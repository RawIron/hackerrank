#include <iostream>
#include <algorithm>
#include <vector>
#include <functional>

using namespace std;


class Message {
public:
    explicit Message(const string& text);
    bool operator<(const Message& rhs) const;
    bool operator=(const Message& rhs) const;
    const string& text() const;
private:
    inline static int seq_id_{ 0 };
    const int id_;
    const string payload_;
};


Message::Message(const string& text) : id_{seq_id_}, payload_{text} {
    ++seq_id_;
}

bool Message::operator<(const Message& rhs) const {
    if (id_ < rhs.id_) { return true; }
    else { return false; }
}

bool Message::operator=(const Message& rhs) const {
    if (id_ == rhs.id_) { return true; }
    else { return false; }
}

const string& Message::text() const {
    return payload_;
}


namespace MessageFactory {
    Message create(const string& text) {
        return Message(text);
    }
};


class Recipient {
public:
    void receive(const Message& msg);
    void process(const function<void(const string&)>& func);
private:
    vector<Message> messages_;
};

void Recipient::receive(const Message& msg) {
    messages_.push_back(msg);
}

void Recipient::process(const function<void(const string&)>& func) {
    sort(messages_.begin(), messages_.end());
    for (auto& msg : messages_) {
        func(msg.text());
    }
    messages_.clear();
}


namespace Network {
    // simulates a network where
    // sent messages can arrive in any order
    void send(vector<Message> messages, Recipient& recipient) {
        random_shuffle(messages.begin(), messages.end());
        for (auto msg : messages) {
            recipient.receive(msg);
        }
    }
};


int main() {
    vector<Message> messages;
    string text;
    while (getline(cin, text)) {
        messages.push_back(MessageFactory::create(text));
    }

    Recipient recipient;
    Network::send(messages, recipient);
    recipient.process( [](const string& payload) { cout << payload << endl; } );
}
