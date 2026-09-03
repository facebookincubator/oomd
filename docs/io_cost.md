# IO Cost

IO cost is one of the metrics that oomd collects periodically. Plugins can use
it to decide whether an alarm must start or which cgroup must be cleaned up.
This document describes I/O cost in oomd.

## What is IO Cost

IO cost is a unitless metric that estimates the load that a cgroup puts on
selected I/O devices. The model uses read, write, and trim operation counts and
byte counts.

In order to calculate IO cost, oomd reads the
[`io.stat`](https://facebookmicrosites.github.io/cgroup2/docs/io-controller.html#interface-files)
file in each cgroup. It calculates a weighted cumulative value from the I/O
operation and byte counters. The difference between two consecutive samples is
the I/O cost for one event-loop interval. oomd does not divide this difference
by the elapsed time.

By default, these are the coefficients for HDD and SSD devices:
```
static const struct Oomd::IOCostCoeffs default_hdd_coeffs = {
    .read_iops = 1.31e-3,
    .readbw = 1.13e-7,
    .write_iops = 2.58e-1,
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
These coefficients came from experiments with devices on Facebook servers.
They can be inaccurate for other I/O devices. Test and tune the coefficients
for your devices.

## How to Configure IO Cost

IO cost in oomd is configured by command line arguments:
```
--device DEVS
--hdd-coeffs COEFFS
--ssd-coeffs COEFFS
```

### `--device DEVS`

This option specifies the root devices that contribute to the I/O cost. The
`io.stat` file can have one line for each device that the cgroup used. oomd uses
only the lines for the selected root devices.

This option expects a comma-separated list of `<major>:<minor>` device pairs.
For example, `252:1,253:1` selects two devices. oomd sums the I/O data for both
devices when it calculates the cost.

### `--hdd-coeffs COEFFS`

This option specifies an alternative to the `default_hdd_coeffs` shown above.
It expects a comma-separated list of numeric values. The values apply to read
operations, read bytes, write operations, write bytes, trim operations, and
trim bytes, in that order. See `std::stod` for supported numeric formats. If
fewer than six values are present, the remaining coefficients are zero. oomd
applies all six coefficients. The default HDD trim coefficients are zero.

### `--ssd-coeffs COEFFS`

This option specifies an alternative to the `default_ssd_coeffs` shown above. It
has the same format as `--hdd-coeffs` but coefficients for trim should be
included.
