---
id: overview
title: Overview of oomd
sidebar_label: Overview of oomd
---

Out of memory killing has historically happened inside kernel space. However, if a system runs out of physical memory, the Linux kernel is forced to OOM kill one or more processes. This is typically a slow and painful process because the kernel spends an unbounded amount of time swapping in and out pages and evicting the page cache. Furthermore, configuring policy is not very flexible while being somewhat complicated.

## Introducing oomd

oomd aims to solve this problem in userspace. oomd takes corrective action in userspace before an OOM occurs in kernel space. Corrective action is configured via a flexible plugin system, in which custom code can be written. By default, this involves killing offending processes. This enables an unparalleled level of flexibility where each workload can have custom protection rules. Furthermore, time spent churning pages in kernelspace is minimized.

## Getting started

See the following sections in the [oomd README](https://github.com/facebookincubator/oomd/blob/master/README.md) for instructions on getting started with and configuring oomd:

- [Build and install oomd](https://github.com/facebookincubator/oomd/blob/master/README.md#building-and-installing)
- [Configure oomd](https://github.com/facebookincubator/oomd/blob/master/README.md#configuring-oomd)

## Case Study

See the [case study](/oomd/docs/oomd-casestudy.html) for information on how oomd is creating significant memory utilization gains in production in Facebook's data centers. 

## Related tools

oomd works with uses several other Linux tools to provide advanced oom-killing capabilities. Be sure to check out the following components for deeper insight and more info on how to get the most benefits from oomd:

- **PSI:** oomd uses `memory.pressure` metrics that are part of the [Pressure Stall Information (PSI)](https://facebookmicrosites.github.io/psi/) Linux kernel module to trigger specified actions. 
- **cgroup2:** oomd brings maximum memory utilization benefits to large data centers when used in conjunction with [cgroup2](https://facebookmicrosites.github.io/cgroup2/), a kernel mechanism to group processes and allocate specified amounts of resources to each group. oomd can be configured to kill entire cgroups, rather than discrete processes. 


