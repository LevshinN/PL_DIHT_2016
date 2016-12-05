// Сервер игры крестики-нолики

#ifndef TICTACSERVER_H_
#define TICTACSERVER_H_

#include <Wt\WString>

#include <vector>
#include <map>

#include <boost\noncopyable.hpp>
#include <boost\thread.hpp>

// структура события в игре
struct GameEvent
{
    // Тип события в игре
    enum EEventType
    {
        ETLogin,
        ETLogout,
        ETTryMove,
        EWinCondition
    };

    EEventType  type_;
    Wt::WString user_;
    Wt::WString data_;

    GameEvent(const Wt::WString& user, const Wt::WString& data)
        : type_(ETTryMove)
        , user_(user)
        , data_(data)
    {}

    GameEvent(EEventType type, const Wt::WString& user, const Wt::WString& data = Wt::WString::Empty)
        : type_(type)
        , user_(user)
        , data_(data)
    {}

    const Wt::WString toText(const Wt::WString& user) const;
};

typedef boost::function<void(const GameEvent&)> GameEventCallback;

class TicTacServer : boost::noncopyable
{
public:
    class Client{};

    TicTacServer(Wt::WServer& server);

    bool connect(Client* client, const GameEventCallback& handleEvent);
    bool disconnect(Client* client);

    bool login(const Wt::WString& user);
    bool logout(const Wt::WString& user);

    Wt::WString suggestName();
    Wt::WString getCurrentUser();

    void sendMove(const Wt::WString& user, const Wt::WString& message);

    typedef std::map<Wt::WString, int> Users;
    typedef std::map<int, std::map<int, int>> GameField;

    Users* users();
    GameField* field();

private:
    struct ClientInfo
    {
        std::string sessionID;
        GameEventCallback eventCallback;
    };

    typedef std::map<Client*, ClientInfo> ClientMap;

    Wt::WServer& m_server;
    boost::recursive_mutex m_mutex;
    ClientMap m_clients;
    Users m_users;
    GameField m_field;

    Wt::WString m_currUser;

    void postGameEvent(const GameEvent& event);

    bool winCondition(const Wt::WString& user);
    bool winCell(int x, int y, int mark);
};

#endif //TICTACSERVER_H_