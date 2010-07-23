package com.shorrockin.soc

import java.util.concurrent.Semaphore

/**
 * a cache with functional constructs that can cache objects for a period
 * of time locally, which safeguards against stampeding. While there may
 * be other libraries out there which do similar things, I wasn't aware
 * of any so I stopped cutting and pasting this between projects and put
 * it in a library of it's own.
 *
 * @author Chris Shorrock
 * @param Key the key type used in this cache.
 * @param Value the object type that this cache stores.
 */
class Soc[Key, Value](val rule:Rule[Key]) {
  private var cache    = Map[Key, (rule.Instruction, Value)]()
  private var locks    = Map[Key, Semaphore]()
  
  /**
   * because the locks map is populated using dcl, we would normally synchronize
   * that - but because it's immutable, that obviously doesn't work. this object
   * then serves as something we can synchronize against. might need to re-evaluate
   * this approach
   */
  private case object LocksLock


  /**
   * retrievs or creates the cached object based on the caching policies.
   * when we determine if we should retrieve from the cache or repopulate
   * this is based on the following ruleset:
   *
   *                  cache exist?    cache expired?    lock available?
   * -------------------------------------------------------------------
   * write cache            N                -                 Y
   * wait on write          N                -                 N
   * serve cached           Y                N                 -
   * serve cached           Y                Y                 N
   * write cache            Y                Y                 Y
   * -------------------------------------------------------------------  
   */
  def get(key:Key)(f: => Value):Value = {
    def populate() = {
      val result      = f
      val instruction = rule.instruction(key)
      cache = cache + (key -> (instruction, result))
      result
    }

    isCached(key) match {
      case false => safeTryAcquire(getLock(key)) { 
        // cache does not exist, lock acquired, creating and populating entries.
        case true => populate()

        // cache is does not, but lock could not be acquired, blocking until lock is free
        case false => 
          getLock(key).acquire() // blocks
          cache(key) match {
            case null => throw new IllegalStateException("expected result after cache entry, but nothing was populated")
            case a    => return a._2
          }
      }
      case true  => 
        val cacheTuple  = cache(key)
        val instruction = cacheTuple._1
        val value       = cacheTuple._2
      
        rule.expired(instruction) match {
          // cache exists, is not expired, serve data from cache.
          case false => value

          // cache exists, and expired, get or return stale
          case true => safeTryAcquire(getLock(key)) {
            // cache exists, but is expired, have lock, repopulating cache
            case true => populate()

            // cache exists, expired, could not get lock, returning stale data
            case false => value 
          }
        }
    }
  }


  /**
   * true if the specified key is cached
   */
  def isCached(key:Key) = locks.contains(key) && cache.contains(key)


  /**
   * retrieves the lock for the specifed key, uses DCL to ensure that only
   * one lock is created.
   */
  private def getLock(key:Key) = locks.contains(key) match {
    case true  => locks(key)
    case false =>
      LocksLock.synchronized {
        locks.contains(key) match {
          case true => locks(key)
          case false => 
            locks = locks + (key -> new Semaphore(1))
            locks(key)
        }
      }
  }


  /**
   * manages the lock.release, and performs a try acquire
   */
  private def safeTryAcquire[E](lock:Semaphore)(f:PartialFunction[Boolean, E]) = {
    try {
      f(lock.tryAcquire)
    } finally {
      lock.release()
    }
  }
}


/**
 * defines a cache rules with information on how an object should be
 * cached.
 * @author Chris Shorrock
 */
trait Rule[-Key] {
  type Instruction

  /**
   * creates a new cache instruction for the specified key value
   */
  def instruction(key:Key):Instruction


  /**
   * tests the instruction to determine if this instruction has
   * expired
   */
  def expired(inst:Instruction):Boolean
}


/**
 * a type of rule which causes the entrants to never expire.
 * @author Chris Shorrock
 */
case object NeverExpireRule extends Rule[Any] {
  type Instruction              = NeverExpiresInstruction.type
  def instruction(key:Any)      = NeverExpiresInstruction
  def expired(inst:Instruction) = false

  case object NeverExpiresInstruction
}


/**
 * a type of rule which expires a cached entry after a certain amount of time
 * @author Chris Shorrock
 */
case class TimeExpiredRule(time:Long) extends Rule[Any] {
  type Instruction = TimeExpiredInstruction

  def instruction(key:Any) = new TimeExpiredInstruction(System.currentTimeMillis())

  def expired(inst:TimeExpiredInstruction) = {
    val now = System.currentTimeMillis()
    (now - inst.creation) > time
  }

  case class TimeExpiredInstruction(creation:Long) 
}


