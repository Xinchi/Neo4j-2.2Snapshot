kill $(sudo lsof -t -i:7474);
mvn clean install -DminimalBuild -Dlicense.skip=true -DskipBrowser -U
