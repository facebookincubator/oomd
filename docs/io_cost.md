# IO Cost

IO cost is one of the many metrics (swap, memory, pressure, etc) logged by oomd
periodically, based on which the plugins can decide if an alarm should be
triggered, or which cgroup should be cleaned up. This documentation provides
some background about IO cost in the context of oomd.

## What is IO Cost

IO cost is a unit-less metric measuring how much load is put on some IO devices
by a cgroup. It is based on a simple model using bandwidth and iops of read,
write, and trim to approximate some definition of load.

In order to calculate IO cost, oomd reads the
[`io.stat`](https://facebookmicrosites.github.io/cgroup2/docs/io-controller.html#interface-files)
file in each cgroup, from where the cumulative IOs and bytes of read/write/trim
are obtained. Then the difference between two consecutive intervals divided by
the duration is bandwidth and iops. The dot-product between some coefficients
and the measurements is the IO cost.

By default, these are the coefficients for HDD and SSD devices:
```
static const struct Oomd::IOCostCoeffs default_hdd_coeffs = {
    .read_iops = 2.58e-1,
    .readbw = 1.13e-7,
    .write_iops = 1.31e-3,
    .writebw = 5.04e-7,
    .trim_iops = 0,
    .trimbw = 0,
};
static const struct Oomd::IOCostCoeffs default_ssd_coeffs = {
    .read_iops = 1.21e-2,
    .readbw = 6.25e-7,
    .write_iops = 1.07e-3,
    .writebw = 2.61e-7,
    .trim_iops = 2.37e-2,
    .trimbw = 9.10e-10,
};
```
These number are obtained through experiments with disks running on Facebook
servers. They may not match other IO devices and therefore running experiments
with your own devices is recommended.

## How to Configure IO Cost

IO cost in oomd is configured by command line arguments:
```
--device DEVS
--hdd-coeffs COEFFS
--ssd-coeffs COEFFS
```

### `--device DEVS`

This option tells oomd what are the root devices, or ones that will contribute
to calculating the IO cost. The `io.stat` file can have multiple lines, one for
each IO device that the cgroup has interacted with. Only lines belong to the
root devices will be used to calculated the IO cost.

This option expects `DEVS` in the format of comma separated `<major>:<minor>`
pairs of devices, e.g. `252:1,253:1` for two devices, one with major=252 and
minor=1 and the other with major=253 and minor=1. This will tell oomd to
calculate IO cost by summing the bandwitdh and iops data of both `252:1` and
`253:1`.

### `--hdd-coeffs COEFFS`

This option specifies an alternative to the `default_hdd_coeffs` shown above. It
expects a comma separated list of numeric values, which will be coefficients in
the order of read iops, read bandwidth, write iops, write bandwidth, trim iops,
and trim bandwidth. See `std::stod` for support numeric formats. If less than 6
values are passed, remaining ones are set zero. Coefficients for trim are
likely ignored and should not be passed.

### `--ssd-coeffs COEFFS`

This option specifies an alternative to the `default_ssd_coeffs` shown above. It
has the same format as `--hdd-coeffs` but coefficients for trim should be
included.
