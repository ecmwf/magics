#include <unistd.h>
#include <signal.h>
#include "WebFormat.h"
#include "MetaData.h"

using namespace magics;

namespace magml
{

pair<string, string> cut(const string& in)
{
	string::const_iterator c = in.begin();
	string var;
	string current;
	bool  param = true;

	while ( c != in.end() )
	{
		if ( *c == '='  && param )
		{
			var = current;
			current = "";
			param= false;
		}
		else {
			current.push_back(*c);
		}
		c++;
	}
	return std::make_pair(var, current);
}

void setVariable(const string& in, map<string, string>& variables)
{
	if(in[0] != '-')
	{
		MagLog::warning() << "argument " << in << " ignored."<< std::endl;
		return;
	}
	variables.insert(magml::cut(in.substr(1, in.size())));
}

} //end of namespace magml

int normal_main(int argc, char **argv)
{
  try
    {
	MetaDataVisitor::start();
	if(argc<2)
	{
		std::cout << " Usage: " << argv[0] << " file.json [-varname=varvalue]"<< endl;
		exit (1);
	}

	map<string, string> variables;

	for( int i = 2; i < argc; i++ )
		magml::setVariable(argv[i], WebInterpretor::parameters());

	//for ( int i = 0; i < 10; i++)
	{
		try {
            map<string, string>::const_iterator out = WebInterpretor::parameters().find("timeout");
            int timeout = (out == WebInterpretor::parameters().end() ) ? 0 : tonumber(out->second);
            MagLog::info() << "Time out armed for " << timeout << endl;
            signal(SIGALRM, catch_alarm);
            alarm(timeout);
            WebInterpretor::json(argv[1]);
            alarm(0);
		}
		catch (...) {
			std::cout << argv[0] << " FAILED to dispatch JSON file!"<< endl;
			exit(1);
		}
	}
    }
    catch (MagicsException& e)
    {
		std::cout << "MagJson: Catch Exception " << e << endl;
		exit(1);
    }
}

int server_main(int argc, char **argv)
{
    char line[10240];
    cout << "MAGJSON_SERVER_MODE Ready" << endl;
    for(;;)
    {
        cin.getline(line, sizeof(line));
        cout << "Running " << line << endl;
		try {
            MetaDataVisitor::start();
            WebInterpretor::json(line);
        }
		catch (...) {
			std::cout << argv[0] << " FAILED to dispatch JSON file!"<< endl;
            cout << "CODE 1" << endl << flush;
			exit(1);
		}
        cout << "CODE 0" << endl << flush;
    }
}

int main(int argc, char **argv)
{
    if(getenv("MAGJSON_SERVER_MODE"))
    {
        server_main(argc,argv);
    }
    else
    {
        normal_main(argc,argv);
    }
    exit(0);
}
