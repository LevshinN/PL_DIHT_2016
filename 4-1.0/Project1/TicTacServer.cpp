#include "TicTacServer.h"
#include <Wt/WServer>

using namespace Wt;

const WString GameEvent::toText(const Wt::WString& user) const
{
    switch (type_)
    {
    case ETLogin:
        if (user == user_)
            return WString::fromUTF8("You sucsessfully joined.");
        else
            return WWebWidget::escapeText(user_) + " joined.";

    case ETLogout:
        return  WWebWidget::escapeText(user_) + " left game.";
    case ETTryMove:
        return WWebWidget::escapeText(user_) + " moved";
    case EWinCondition:
        if (user == user_)
            return WString::fromUTF8("You win!!!");
        else
            return WWebWidget::escapeText(user_) + " win!!!";    
    default:
        return "";
    }
}

TicTacServer::TicTacServer(Wt::WServer& server)
    : m_server(server)
{}

bool TicTacServer::connect(Client* client, const GameEventCallback& handleEvent)
{
    boost::recursive_mutex::scoped_lock lock(m_mutex);

    if (m_clients.count(client) == 0)
    {
        ClientInfo clientInfo;

        clientInfo.sessionID = WApplication::instance()->sessionId();
        clientInfo.eventCallback = handleEvent;

        m_clients[client] = clientInfo;

        return true;
    }
    else
    {
        return false;
    }
}

bool TicTacServer::disconnect(Client* client)
{
    boost::recursive_mutex::scoped_lock lock(m_mutex);

    return m_clients.erase(client) == 1;
}

bool TicTacServer::login(const Wt::WString& user)
{
    boost::recursive_mutex::scoped_lock lock(m_mutex);

    int mark = m_users.size();
    m_users[user] = mark + 1;
        
    postGameEvent(GameEvent(GameEvent::ETLogin, user));

    return true;
}

bool TicTacServer::logout(const Wt::WString& user)
{
    boost::recursive_mutex::scoped_lock lock(m_mutex);

    if (m_users.find(user) != m_users.end())
    {
        m_users[user] = false;

        postGameEvent(GameEvent(GameEvent::ETLogout, user));
    }
    return true;
}

Wt::WString TicTacServer::suggestName()
{
    boost::recursive_mutex::scoped_lock lock(m_mutex);

    std::string name = "Player " + boost::lexical_cast<std::string>(m_users.size() + 1);
    WString wname = name;

    return wname;
}

Wt::WString TicTacServer::getCurrentUser()
{
    return m_currUser;
}

void TicTacServer::sendMove(const Wt::WString& user, const Wt::WString& data)
{
    postGameEvent(GameEvent(user, data));
}

void TicTacServer::postGameEvent(const GameEvent& event)
{
    boost::recursive_mutex::scoped_lock lock(m_mutex);

    if (event.type_ == GameEvent::ETLogin)
    {
        if (m_currUser.empty())
        {
            m_currUser = event.user_;
        }
    }

    if (event.type_ == GameEvent::ETTryMove)
    {
        if (event.user_ == m_currUser)
        {
            Users::iterator itUser = m_users.find(m_currUser);
            int mark = itUser->second;

            std::wstring data = event.data_.value();
            std::wstring x = data.substr(0, data.find(L"_"));
            std::wstring y = data.substr(x.size() + 1, data.size());

            int posx = std::stoi(x);
            int posy = std::stoi(y);

            m_field[posx][posy] = mark;

            bool win = winCondition(event.user_);

            if (win)
            {
                postGameEvent(GameEvent(GameEvent::EWinCondition, event.user_));
                return;
            }
            else
            {
                itUser++;
                if (itUser == m_users.end()) itUser = m_users.begin();

                m_currUser = itUser->first;
            }

        }
        else
            return;
    }


    WApplication* pApp = WApplication::instance();

    for (ClientMap::const_iterator itClient = m_clients.begin(); itClient != m_clients.end(); ++itClient)
    {
        if (pApp && pApp->sessionId() == itClient->second.sessionID)
            itClient->second.eventCallback(event);
        else
            m_server.post(itClient->second.sessionID, boost::bind(itClient->second.eventCallback, event));
    }
}

TicTacServer::Users* TicTacServer::users()
{
    boost::recursive_mutex::scoped_lock lock(m_mutex);

    return &m_users;
}

TicTacServer::GameField* TicTacServer::field()
{
    boost::recursive_mutex::scoped_lock lock(m_mutex);

    return &m_field;
}

bool TicTacServer::winCell(int x, int y, int mark )
{
    bool win = true;

    int coordsX[5] = { x, x + 1, x + 2, x + 3, x + 4 };
    int coordsY[5] = { y, y + 1, y + 2, y + 3, y + 4 };

    // check right
    for (int i = 0; i < 5; ++i)
    {
        if (m_field.find(coordsX[i]) == m_field.end())
        {
            win = false;
            break;
        }

        if (m_field[coordsX[i]].find(y) == m_field[coordsX[i]].end())
        {
            win = false;
            break;
        }

        if (m_field[coordsX[i]][y] != mark)
        {
            win = false;
            break;
        }
    }

    if (win)
        return true;
    else
        win = true;

    // check bottom
    for (int i = 0; i < 5; ++i)
    {
        if (m_field.find(x) == m_field.end())
        {
            win = false;
            break;
        }

        if (m_field[x].find(coordsY[i]) == m_field[x].end())
        {
            win = false;
            break;
        }

        if (m_field[x][coordsY[i]] != mark)
        {
            win = false;
            break;
        }
    }

    if (win)
        return true;
    else 
        win = true;

    // check bottom - right
    for (int i = 0; i < 5; ++i)
    {
        if (m_field.find(coordsX[i]) == m_field.end())
        {
            win = false;
            break;
        }

        if (m_field[coordsX[i]].find(coordsY[i]) == m_field[coordsX[i]].end())
        {
            win = false;
            break;
        }

        if (m_field[coordsX[i]][coordsY[i]] != mark)
        {
            win = false;
            break;
        }
    }

    return win;
}

bool TicTacServer::winCondition(const Wt::WString& user)
{
    int mark = m_users[user];

    for (auto row : m_field)
    {
        for (auto cell : row.second)
        {
            if (winCell(row.first, cell.first, mark))
                return true;
        }
    }

    return false;
}