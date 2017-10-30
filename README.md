# rolespike

Simple CRUD Service (based on my suggested API for the roles service). Not for production use.

Currently only runs on an in-memory database.

## All the instructions you need

```sbt test```

```sbt run```

```sbt alltests:test```

The last one will run the MySQL tests as well. It looks for connection details in environment variables, with the following very insecure
defaults:

```
MYSQL_CONNECTION_URI=jdbc:mysql://localhost:3306/roletestdb?useSSL=false
MYSQL_USERNAME=root
MYSQL_PASSWORD=secret
```

## Notes

Application code is in 'fandom.roles'. Non-service-specific supporting code is in the 'fandom' package.
