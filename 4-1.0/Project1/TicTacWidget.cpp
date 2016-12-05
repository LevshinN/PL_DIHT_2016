#pragma once

#include "TicTacWidget.h"

#include <Wt/WVBoxLayout>
#include <Wt/WHBoxLayout>
#include <Wt/WText>
#include <Wt/WPushButton>

using namespace Wt;

#define FIELD_SIZE 20
#define CELL_SIZE 50

TicTacWidget::TicTacWidget(TicTacServer& server, Wt::WContainerWidget* parent)
    : WContainerWidget(parent)
    , m_server(server)
    , m_gameActive(false)
    , m_userList(0)
    , m_field(0)
{
    m_user = m_server.suggestName();
    letLogin();
    login();
}

TicTacWidget::~TicTacWidget()
{
    logout();
}

void TicTacWidget::connect()
{
    if (m_server.connect(this, boost::bind(&TicTacWidget::processGameEvent, this, _1)))
        Wt::WApplication::instance()->enableUpdates(true);
}
void TicTacWidget::disconnect()
{
    if (m_server.disconnect(this))
        Wt::WApplication::instance()->enableUpdates(false);
}

void TicTacWidget::letLogin()
{
    clear();

    WVBoxLayout *vLayout = new WVBoxLayout();
    setLayout(vLayout);

    WHBoxLayout* hLayout = new WHBoxLayout();
    vLayout->addLayout(hLayout, 0, AlignTop | AlignLeft);

    vLayout->addWidget(m_statusMsg = new WText());
    m_statusMsg->setTextFormat(PlainText);
}

void TicTacWidget::login()
{
    if (!loggedIn()) {
        WString name = m_user;

        if (!startGame(name))
            m_statusMsg->setText("Sorry, '" + escapeText(name) +
            "' is already connected.");
    }
}

void TicTacWidget::logout()
{
    if (loggedIn()) {
        m_gameActive = false;
        m_server.logout(m_user);
        disconnect();

        letLogin();
    }
}

void TicTacWidget::createLayout(Wt::WLayout* field, Wt::WWidget* userList)
{
    /*
    * Create a horizontal layout, which will hold 2 columns,
    * organized like this:
    *
    * WVBoxLayout
    * --------------------------------------------
    * |  Status Message              |           |
    * |----------------------------- |           |
    * |                              | userList  |
    * |   field (N x N)              |           |
    * |                              |           |
    * --------------------------------------------
    */

    // Create a horizontal layout for the field | userslist.
    WHBoxLayout *hLayout = new WHBoxLayout();

    // Add widget to horizontal layout with stretch = 1

    WVBoxLayout* vLayout = new WVBoxLayout();

    m_statusMsg = new WText();
    m_statusMsg->setTextFormat(PlainText);
    m_statusMsg->setStyleClass("game-status");
    m_statusMsg->setText("Wellcome");

    vLayout->addWidget(m_statusMsg);
    vLayout->addLayout(field);

    hLayout->addLayout(vLayout);

    // Add another widget to horizontal layout with stretch = 0
    hLayout->addWidget(userList);
    userList->setStyleClass("game-users");

    setLayout(hLayout);
    setHeight(FIELD_SIZE * 30);
}

bool TicTacWidget::loggedIn() const
{
    return m_gameActive;
}

void TicTacWidget::render(WFlags<RenderFlag> flags)
{
    WContainerWidget::render(flags);
}

bool TicTacWidget::startGame(const WString& user)
{
    if (m_server.login(user)) {
        m_gameActive = true;
        connect();

        m_user = user;

        clear();

        WHBoxLayout *field = new WHBoxLayout();

        int N = FIELD_SIZE;

        m_field.resize(N);
        for (int i = 0; i < N; ++i)
        {
            m_field[i].resize(N);
            WVBoxLayout *verLayout = new WVBoxLayout();
            
            field->addLayout(verLayout);
            for (int j = 0; j < N; ++j)
            {
                m_field[i][j] = new WPushButton(" ");
                m_field[i][j]->setId("cell_" + boost::lexical_cast<std::string>(i) + "_" + boost::lexical_cast<std::string>(j));
                m_field[i][j]->setStyleClass("game-cell");
                m_field[i][j]->setWidth(CELL_SIZE);
                m_field[i][j]->setHeight(CELL_SIZE);
                m_field[i][j]->clicked().connect(this, &TicTacWidget::sendMove);
                verLayout->addWidget(m_field[i][j]);
            }
        }

        m_userList = new WContainerWidget();

        m_userList->setOverflow(WContainerWidget::OverflowAuto);

        createLayout(field, m_userList);

        if (!m_userList->parent()) {
            delete m_userList;
            m_userList = 0;
        }

        updateUsers();
        updateField();

        return true;
    }
    else
        return false;
}

void TicTacWidget::sendMove()
{
    if (!m_gameActive)
        return;

    std::string id = WApplication::sender()->id();

    std::string delimiter = "_";
    std::string token = id.substr(0, id.find(delimiter));

    if (token != "cell")
        return;

    WString move = id.substr(token.size() + 1, id.size());

    m_server.sendMove(m_user, move);
}

void TicTacWidget::updateUsers()
{
    if (m_userList) {
        m_userList->clear();

        TicTacServer::Users* users = m_server.users();

        UserMap oldUsers = m_users;
        m_users.clear();

        WString currUser = m_server.getCurrentUser();

        for (TicTacServer::Users::iterator itUser = users->begin();
            itUser != users->end(); ++itUser) {

            if (!itUser->second)
                continue;

            WText *w = new WText(escapeText(itUser->first), m_userList);
            w->setInline(false);

            m_users[itUser->first] = true;

            if (itUser->first == m_user)
                w->addStyleClass("game-self");

            if (itUser->first == currUser)
                w->addStyleClass("game-current");
        }
    }
}

void TicTacWidget::updateUser()
{
    WText* b = dynamic_cast<WText*>(sender());
    m_users[b->text()] = true;
}

void TicTacWidget::updateField()
{
    TicTacServer::GameField* pGameField = m_server.field();

    for (int i = 0; i < m_field.size(); ++i)
    {
        for (int j = 0; j < m_field[i].size(); ++j)
        {
            if (pGameField->find(i) == pGameField->end()) continue;
            if (pGameField->at(i).find(j) == pGameField->at(i).end()) continue;

            int mark = pGameField->at(i)[j];
            std::string s;

            if (mark == 1)
                s = "X";
            else
                s = boost::lexical_cast<std::string>(mark - 2);

            m_field[i][j]->setText(s);
        }
    }

}

void TicTacWidget::processGameEvent(const GameEvent& event)
{
    WApplication *app = WApplication::instance();

    updateUsers();

    app->triggerUpdate();

    m_statusMsg->setText(event.toText(m_user));

    if (!loggedIn())
        return;

    updateField();

    if (event.type_ == GameEvent::EWinCondition)
    {
        m_gameActive = false;
    }


}

