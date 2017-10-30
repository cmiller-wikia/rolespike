# rolespike

Simple CRUD Service (based on my suggested API for the roles service). Not for production use.

Currently only runs on an in-memory database.

## All the instructions you need

```sbt rolesvc/test```

```sbt rolesvc/run```

```sbt rolesvc/alltests:test```

The last one will run the MySQL tests as well. It looks for connection details in environment variables, with the following very insecure
defaults:

```
MYSQL_CONNECTION_URI=jdbc:mysql://localhost:3306/roletestdb?useSSL=false
MYSQL_USERNAME=root
MYSQL_PASSWORD=secret
```

## Organisation

```
/base-libs          -- generic service code that is not fandom-specific
/fandom-integration -- integration code for fandom services infrastructure
/role-service       -- an example service implementation
