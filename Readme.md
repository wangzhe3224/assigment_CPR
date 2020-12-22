# How to run 

`erl -make` to compile.

`erl -pa ebin/` to start Erlang shell.

Run a demo case of the system: `cases_test:demo_run().`

Run single APIs tests: 

- `cases_test:planner_api_test().`
- `cases_test:manager_api_test().`
- `cases_test:vehicle_api_test().`
- `cases_test:vehicle_sup_test().`

Exit erlang shell, and run top_sup tests:

- `cases_test:top_sup_test().`
