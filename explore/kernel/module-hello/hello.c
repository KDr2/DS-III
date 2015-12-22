#include <linux/init.h>
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/fs.h>
#include <linux/proc_fs.h>
#include <linux/seq_file.h>
#include <linux/jiffies.h>

extern unsigned int tsc_khz;


static int timer_and_clock_proc_show(struct seq_file *m, void *v)
{
    seq_printf(m, "TSC_KHZ: %lu.%03lu MHz\n",
        (unsigned long)tsc_khz / 1000,
        (unsigned long)tsc_khz % 1000);
    seq_printf(m, "JIFFIES: %llu\n",
        (unsigned long long) get_jiffies_64());
    return 0;
}

static int timer_and_clock_proc_open(struct inode *inode, struct file *file)
{
    return single_open(file, timer_and_clock_proc_show, NULL);
}

static const struct file_operations timer_and_clock_proc_fops = {
    .owner      = THIS_MODULE,
    .open       = timer_and_clock_proc_open,
    .read       = seq_read,
    .llseek     = seq_lseek,
    .release    = single_release,
};


static int hello_init(void){
    printk(KERN_ERR "init hello\n");
    proc_create("timer_and_clock", 0, NULL, &timer_and_clock_proc_fops);
    return 0;
}

static void hello_exit(void){
    remove_proc_entry("timer_and_clock", NULL);
    printk(KERN_ERR "exit hello\n");
}

module_init(hello_init);
module_exit(hello_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("KDr2");
MODULE_DESCRIPTION("Hello");
