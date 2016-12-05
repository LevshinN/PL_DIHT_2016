#ifndef TICTACWIDGET_H
#define TICTACWIDGET_H

#include <Wt/WContainerWidget>
#include <Wt/WJavaScript>
#include <Wt/WApplication>
#include <Wt/WPushButton>


#include "TicTacServer.h"

class GameEvent;

class TicTacWidget
    : public Wt::WContainerWidget
    , public TicTacServer::Client
{
public:

    TicTacWidget(TicTacServer& server, Wt::WContainerWidget* parent = 0);

    ~TicTacWidget();

    void connect();
    void disconnect();

    void letLogin();

    bool startGame(const Wt::WString& user);

    void login();
    void logout();

    TicTacServer& getServer() { return m_server; }
    int userCount() { return m_users.size(); }

    const Wt::WString& userName() const { return m_user; }

protected:
    virtual void createLayout(Wt::WLayout* field, Wt::WWidget* userList);
    virtual void updateUsers();

    virtual void render(Wt::WFlags<Wt::RenderFlag> flags);

protected:
    bool loggedIn() const;

private:
    typedef std::map<Wt::WString, bool> UserMap;
    UserMap m_users;

    TicTacServer& m_server;
    bool m_gameActive;

    Wt::JSlot   m_clearInput;

    Wt::WString m_user;

    Wt::WText* m_statusMsg;

    
    typedef std::vector<std::vector<Wt::WPushButton*>> Field;

    Field m_field;
    Wt::WContainerWidget* m_userList;

    void sendMove();
    void updateUser();
    void updateField();

    void processGameEvent(const GameEvent& event);
};

#endif //TICTACWIDGET_H