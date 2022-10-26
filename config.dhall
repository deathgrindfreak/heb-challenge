let DBConfig = { dbName : Text, dbHost : Text, dbUser : Text, dbPass : Text }

let database
    : DBConfig
    = { dbName = "image"
      , dbHost = "localhost"
      , dbUser = "image_user"
      , dbPass = "image_db_pass"
      }

in  { api.port = +3000
    , imagga =
      { apiKey = ./imagga_key as Text, apiSecret = ./imagga_secret as Text }
    , database
    }
