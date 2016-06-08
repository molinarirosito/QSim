mvn install
mvn assembly:assembly -DdescriptorId=jar-with-dependencies
mv target/QSim.jar build/

