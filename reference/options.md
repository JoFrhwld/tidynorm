# tidynorm Options

Options to control the verbosity of tidynorm functions. The convenience
function
[`tidynorm_options()`](https://jofrhwld.github.io/tidynorm/reference/tidynorm_options.md)
will set these options within your current session. For a common
behavior across R sessions, set the environment variables as described
below. If you've silenced informational messages and want to double
check what normalization steps have been taken, use
[`check_norm()`](https://jofrhwld.github.io/tidynorm/reference/check_norm.md).

## Checking Option Values

Option values specific to `tidynorm` can be accessed by passing the
package name to `env`.

    options::opts(env = "tidynorm")

    options::opt(x, default, env = "tidynorm")

## Options

- tidynorm.silent:

  default:

  :   FALSE

  option:

  :   tidynorm.silent

  envvar:

  :   R_TIDYNORM_TIDYNORM_SILENT (evaluated if possible, raw string
      otherwise)

- tidynorm.warnings:

  default:

  :   TRUE

  option:

  :   tidynorm.warnings

  envvar:

  :   R_TIDYNORM_TIDYNORM_WARNINGS (evaluated if possible, raw string
      otherwise)

## See also

options getOption Sys.setenv Sys.getenv
