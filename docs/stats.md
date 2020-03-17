# Structured stats collection

The structured stats collection is a threadsafe key-value store, with `std::string` for keys and `int` for values.

It can be also be queried from a different process using the StatsClient class.


## Internal API

### Get stats
  `std::unordered_map<std::string, int> getStats()`


  Returns an unordered map copy of the current stats.


### Increment a key-value pair in stats
  `int setStats(const std::string& key, int val)`


  This increments the value of the corresponding key by val.

  Returns 0 upon success, and 1 on error.


### Set a key-value pair in stats
  `int setStats(const std::string& key, int val)`


  Sets the corresponding key in stats to val.

  Returns 0 upon success, and 1 on error.


### Reset stats
  `int Oomd::resetStats()`


  Sets the value of all existing key-value pairs in the stats collection
  to 0.


  Returns 0 upon success, and 1 on error.



## External interface

  Command line flags:

##### `-d`

  Dumps all accumulated stats to stdout in a JSON string.
  
    $ /path/to/binary -d
    {
     "oomd.kills_structured" : 1,
     "oomd.restarts_structured" : 2
    }
    $ /path/to/binary -d | jq '.["oomd.kills_structured"]'
    1


#####`-r`

  Reset stats by setting all values to 0.

Notes: If both `-d` and `-r` are included, `-d` is completed first.
