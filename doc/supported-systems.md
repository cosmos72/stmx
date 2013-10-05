STMX
======

Supported systems
-----------------

The following table summarizes all the known Common Lisp implementations
where STMX has been tested.

<table border="1">
 <tr><th colspan="3" rowspan="3"></th><th colspan="4">Linux distributions</th></tr>

 <tr>    <th colspan="2">Debian 7.0</th><th>Ubuntu 12.04LTS</th><th>Raspbian</th></tr>
 <tr>    <th>x86_64</th><th>x86</th><th>x86</th><th>armhf (Raspberry Pi)</th></tr>

 <tr><th rowspan="2">SBCL</th><th>1.1.11</th><th>x86_64</th><td>ok</td></tr>

 <tr>                         <th>1.0.55.0</th><th>x86</th><td></td><td>ok</td><td>ok</td></tr>

 <tr><th>ABCL</th><th>1.1.1</th><th>OpenJDK 6b27</th><td>ok</td></tr>

 <tr><th rowspan="3">CCL</th><th rowspan="2">1.9-r15769</th><th>x86_64</th><td>ok</td></tr>
 <tr>                        <th>x86   </th><td>ok</td><td>ok</td><td>ok</td></tr>
 <tr>                        <th>1.9-dev-r15475M-trunk</th><th>armhf </th><th></th><th></th><th></th><td>ok</td></tr>

 <tr><th rowspan="2">CMUCL</th><th>20c Unicode</th><th>x86</th><td>???</td><td>ok, need [1]</td><td>ok, need [1]</td></tr>
 <tr>                          <th>20d Unicode</th><th>x86</th><td>???</td><td>???</td><td>???</td></tr>

 <tr><th rowspan="2">ECL</th><th rowspan="2">13.5.1</th><th>x86_64</th><td>fail</td></tr>
 <tr>                        <th>x86   </th><td></td><td>fail</td><td>fail</td></tr>

</table>

[1] all tested CMUCL versions need to be started with option -fpu x87,
    otherwise the following problems will happen on CPUs that support SSE2 or later:
    a) CMUCL 20c fails to load the ASDF provided by quicklisp (the builtin one is too old)
       with a "division by zero" error.
    b) all tested CMUCL versions sometimes hang in STMX test suite
