#pragma once;

#include "TicTacApplication.h"
#include "TicTacWidget.h"

#include <Wt/WTimer>

using namespace Wt;

TicTacApplication::TicTacApplication(const WEnvironment& env, TicTacServer& server)
    : WApplication(env)
    , m_server(server)
    , m_env(env)
{
    setTitle("Tic Tac Toe Ultimate Edition 2K16");
    useStyleSheet("tictacapp.css");

    messageResourceBundle().use(appRoot() + "game");

    jsTest();

    root()->addWidget(new WText(WString::tr("introduction")));

    TicTacWidget* gameWidget = new TicTacWidget(server, root());
    gameWidget->setStyleClass("game");

    //root()->addWidget(new WText(WString::tr("details")));
}

void TicTacApplication::jsTest()
{
    if (!m_env.javaScript()){
        m_jsError = new WText(WString::tr("serverpushwarning"), root());

        // The 5 second timer is a fallback for real server push. The updated
        // server state will piggy back on the response to this timeout.
        m_timer = new Wt::WTimer(root());
        m_timer->setInterval(5000);
        m_timer->timeout().connect(this, &TicTacApplication::emptyFunc);
        m_timer->start();
    }
}

void TicTacApplication::emptyFunc()
{}

GameWidget::GameWidget(const WEnvironment& env, TicTacServer& server)
    : WApplication(env)
    , m_login(this, "login")
{
    setCssTheme("");
    useStyleSheet("gamewidget.css");

    messageResourceBundle().use(appRoot() + "game");
}