#ifndef TICTACAPPLICATION_H
#define TICTACAPPLICATION_H

#include <Wt/WApplication>
#include <Wt/WEnvironment>
#include <Wt/WContainerWidget>
#include <Wt/WText>

#include "TicTacServer.h"

class TicTacApplication : public Wt::WApplication
{
public:
    TicTacApplication(const Wt::WEnvironment& env, TicTacServer& server);

private:
    TicTacServer& m_server;
    Wt::WText* m_jsError;
    const Wt::WEnvironment& m_env;
    Wt::WTimer* m_timer;

    void jsTest();
    void emptyFunc();
};

class GameWidget : public Wt::WApplication
{
public:
    GameWidget(const Wt::WEnvironment& env, TicTacServer& server);

private:
    Wt::JSignal<Wt::WString> m_login;
};


#endif // TICTACAPPLICATION_H