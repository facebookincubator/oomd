---
id: oomd-casestudy
title: oomd at Facebook
sidebar_label: Case study: oomd at Facebook
---

Sandcastle is a system that builds, tests, and lands code, and is one of the largest services running at Facebook. 

The Sandcastle team's servers were running out of memory and their jobs were getting repeatedly OOM-killed. About 5% of Sandcastle hosts rebooted unexpectedly every day. Scaling Sandcastle by simply adding new servers was no longer a possibility due to the crunch on hardware resources. 

## The problem

Sandcastle machines use Tupperware, Facebook's in-house containerization solution, to run worker tasks that repeatedly fetch build jobs from a queue and run them. Sandcastle was using memory from *tmpfs* (an in-memory temporary filesystem) for the build jobs. When the build processes were killed they didn’t release their memory because the system’s *tmpfs* was bind mounted to each  worker.
<p style="margin-top: 1.3em"></p>
<div style="text-align: center;"><img src = "/oomd/docs/assets/sandcastle-before.png"/></div> 
<p style="margin-top: 1.3em"></p>
Another complication was that the kernel OOM killer would kill just one of a build job’s processes. Since the build job can't function without the dead process, it’s left in an undefined state. In such cases, all running jobs must be canceled and retried, taking nearly an hour before the host is up again.

## The solution

The team wanted to create a cgroup architecture with the following objectives:

- Make OOM kills immediately free the *tmpfs* space the build job was using.
- Make OOM kills kill an entire build job, instead of just one process within it.

Experiments and testing led the team to the following solution:

1. The worker contains each build job it runs inside its own sub-cgroup. All the build job processes are contained in their own cgroup in `workload.slice`.
1. [*oomd*](/cgroup2/docs/memory-strategies.html#oomd-memory-pressure-based-oom) is configured to watch per-job cgroups.
1. Each build job gets its own mount namespace and *tmpfs* mount, instead of living off the worker’s mount namespace and bind mounting the system’s *tmpfs*.
<p style="margin-top: 1.3em"></p>
<div style="text-align: center;"><img src = "/oomd/docs/assets/sandcastle-after.png"/></div> 
<p style="margin-top: 1.3em"></p>

With each build job contained in its own cgroup with ***oomd*** enabled, ***oomd*** kills the entire cgroup and all its processes at once. 

Because the mount namespace is only pinned by the processes in that cgroup, the mount namespace gets released, which in turn frees the `tmpfs` instance the build job was using.

Tupperware no longer needs to clean up the task and restart it, since only the build job is killed instead of the worker itself. The Sandcastle worker notices the build job terminated abnormally and moves on to fetching the next job as quickly as it can. 

## oomd in action

The chart below shows a real-world Sandcastle memory usage spike.

<div style="text-align: center;"><img src = "/oomd/docs/assets/scuba-chart.png"/></div> 

The corresponding *oomd* output log (located in `/var/log/messages`) shows the Sandcastle build job getting killed as a result of the spike in memory pressure:

<div style="text-align: center;"><img src = "/oomd/docs/assets/oomd-output.png"/></div> 

## Results

This solution resulted in a big impact win for the Sandcastle team: since implementing *oomd*, the unexpected server reboot rate dropped from 5% to less than 0.5%. Post-OOM downtimes for Sandcastle machines went from tens of minutes to just seconds, driving a 10% capacity gain. 

The increased reliability also significantly increased utilization, with a 35% increase in build jobs per host.