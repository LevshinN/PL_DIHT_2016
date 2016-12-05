#include <Wt/WApplication>
#include <Wt/WContainerWidget>
#include <Wt/WEnvironment>
#include <Wt/WPushButton>
#include <Wt/WServer>
#include <Wt/WText>
#include <Wt/WTimer>

#include "TicTacServer.h"
#include "TicTacApplication.h"

using namespace Wt;



WApplication *createApplication(const WEnvironment& env,
    TicTacServer& server)
{
    return new TicTacApplication(env, server);
}

WApplication *createWidget(const WEnvironment& env, TicTacServer& server)
{
    return new GameWidget(env, server);
}

int main(int argc, char** argv)
{
    WServer server(argc, argv, WTHTTP_CONFIGURATION);
    TicTacServer gameServer(server);

    server.addEntryPoint(Wt::Application,
        boost::bind(createApplication, _1,
        boost::ref(gameServer)));
    server.addEntryPoint(Wt::WidgetSet,
        boost::bind(createWidget, _1,
        boost::ref(gameServer)), "/game.js");

    if (server.start()) 
    {
        int sig = Wt::WServer::waitForShutdown();
        std::cerr << "Shutting down: (signal = " << sig << ")" << std::endl;
        server.stop();
    }
}