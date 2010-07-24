package com.shorrockin.soc

import org.specs.Specification

class SocSpecification extends Specification {
  def log(msg:String) { /* println("[info] " + msg) */ }

  "soc" should {
    "be able to cache based on rules, like never expire for example" in {
      var counter = 0
      val cache   = new Soc[String, Int](NeverExpireRule)
      def get     = cache.get("counter") {
        counter = counter + 1
        counter
      }

      get() must beEqual(1)
      get() must beEqual(1)
    }


    "be expire data when the rules specified" in {
      var counter = 0
      val rule    = new ExpireWhenISaySo(false)
      val cache   = new Soc[String, Int](rule)

      def get() = cache.get("counter") {
        counter = counter + 1
        counter
      }

      get() must beEqual(1)
      get() must beEqual(1)
      rule.shouldExpire = true
      get() must beEqual(2)
      get() must beEqual(3)
      rule.shouldExpire = false
      get() must beEqual(3)
    }


    "be able to initially block until the initial cache is written" in {
      val rule      = new ExpireWhenISaySo(false)
      val cache     = new Soc[String, String](rule)
      val waiter    = "the waiter"
      var retrieved = "initial"
      var t1Blocked = true
      var t2Blocked = true

      val t1 = new Thread() {
        override def run() {
          retrieved = cache.get("test") {
            log("blocking on thread 1")
            waiter.synchronized { waiter.wait() }
            log("resulming on thread 1, returning")
            "thread 1"
          }
          log("exiting thread 1")
          t1Blocked = false
        }
      }

      val t2 = new Thread() {
        override def run() {
          retrieved = cache.get("test") { 
            log("in thread 2, should never happen")
            "thread 2" 
          }
          log("exiting thread 2")
          t2Blocked = false
        } 
      }

      log("starting up threads, t1 should block, t2 should indirectly block")
      t1.start() // should start - then block 
      Thread.sleep(15) // gives thread 1 enough time to start and block
      t2.start() // should block until t1 finishes.

      retrieved must beEqual("initial")
      t1Blocked must beEqual(true)
      t2Blocked must beEqual(true)

      log("notifying threads to continue")
      waiter.synchronized { waiter.notify() }
      Thread.sleep(15) // give it a second
      retrieved must beEqual("thread 1")
      t1Blocked must beEqual(false)
      t2Blocked must beEqual(false)
      
    }


    "be able to serve stale data while cache is being repopulated" in {
      val rule    = ExpireWhenISaySo(false)
      val cache   = new Soc[String, String](rule)
      val waiter  = "the.waiter"
      def get()   = cache.get("test") { "normal" }
      
      get() must beEqual("normal")

      val blocker = new Thread() {
        override def run() {
          cache.get("test") { 
            waiter.synchronized { waiter.wait() }
            "from-thread"
          }
        }
      }

      rule.shouldExpire = true
      blocker.start()
      Thread.sleep(15) // allow waiter to wait and block
      get() must beEqual("normal") // should return stale data

      waiter.synchronized { waiter.notify() } // allow it to finish
      Thread.sleep(15)

      rule.shouldExpire = false
      get() must beEqual("from-thread") // should of populated with long running populate
      

    }
  }

  case class ExpireWhenISaySo(initial:Boolean = false) extends Rule[Any] {
    type Instruction = ExpireWhenISaySoInstruction.type
    var shouldExpire = initial
    def instruction(key:Any) = ExpireWhenISaySoInstruction
    def expired(inst:Instruction) = shouldExpire

    case object ExpireWhenISaySoInstruction
  }
}
