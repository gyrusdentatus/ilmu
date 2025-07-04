#!/usr/bin/env python3
"""
Local Memory System for Vegur Agents

Following Opus's practical guidance:
- SQLite + content addressing
- Simple deduplication
- Measure real savings
- No distributed nonsense
"""

import sqlite3
import hashlib
import json
import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

class LocalMemory:
    """Content-addressed local memory storage with deduplication"""
    
    def __init__(self, memory_dir: Optional[Path] = None):
        if memory_dir is None:
            memory_dir = Path.home() / ".vegur"
        
        self.memory_dir = Path(memory_dir)
        self.memory_dir.mkdir(parents=True, exist_ok=True)
        
        self.db_path = self.memory_dir / "memory.db"
        self.db = sqlite3.connect(str(self.db_path), check_same_thread=False)
        self._init_schema()
        
        # Track metrics
        self.stats = {
            "stores": 0,
            "retrievals": 0, 
            "deduplications": 0,
            "bytes_saved": 0
        }
    
    def _init_schema(self):
        """Initialize SQLite schema for content-addressed storage"""
        self.db.executescript("""
            CREATE TABLE IF NOT EXISTS memories (
                hash_id TEXT PRIMARY KEY,
                content TEXT NOT NULL,
                content_type TEXT,
                first_stored REAL,
                access_count INTEGER DEFAULT 1,
                last_accessed REAL,
                size_bytes INTEGER
            );
            
            CREATE TABLE IF NOT EXISTS memory_refs (
                ref_id TEXT PRIMARY KEY,
                hash_id TEXT,
                agent_id TEXT,
                created REAL,
                metadata TEXT,
                FOREIGN KEY (hash_id) REFERENCES memories (hash_id)
            );
            
            CREATE INDEX IF NOT EXISTS idx_agent_refs ON memory_refs (agent_id);
            CREATE INDEX IF NOT EXISTS idx_content_type ON memories (content_type);
            CREATE INDEX IF NOT EXISTS idx_last_accessed ON memories (last_accessed);
        """)
        self.db.commit()
    
    def _content_hash(self, content: Any) -> str:
        """Generate SHA-256 hash for content addressing"""
        if isinstance(content, str):
            content_bytes = content.encode('utf-8')
        else:
            content_bytes = json.dumps(content, sort_keys=True).encode('utf-8')
        
        return hashlib.sha256(content_bytes).hexdigest()
    
    def store(self, content: Any, content_type: str = "unknown", 
              agent_id: str = "system", metadata: Dict = None) -> Tuple[str, bool]:
        """
        Store content with deduplication
        
        Returns:
            (hash_id, was_deduplicated)
        """
        self.stats["stores"] += 1
        
        # Convert content to storable format
        if isinstance(content, str):
            content_str = content
        else:
            content_str = json.dumps(content, sort_keys=True)
        
        hash_id = self._content_hash(content_str)
        content_size = len(content_str.encode('utf-8'))
        
        # Check if content already exists
        existing = self.db.execute(
            "SELECT hash_id, size_bytes FROM memories WHERE hash_id = ?",
            (hash_id,)
        ).fetchone()
        
        was_deduplicated = existing is not None
        
        if was_deduplicated:
            # Update access stats for existing content
            self.db.execute("""
                UPDATE memories 
                SET access_count = access_count + 1, 
                    last_accessed = ?
                WHERE hash_id = ?
            """, (time.time(), hash_id))
            
            self.stats["deduplications"] += 1
            self.stats["bytes_saved"] += content_size
            
        else:
            # Store new content
            self.db.execute("""
                INSERT INTO memories 
                (hash_id, content, content_type, first_stored, last_accessed, size_bytes)
                VALUES (?, ?, ?, ?, ?, ?)
            """, (hash_id, content_str, content_type, time.time(), time.time(), content_size))
        
        # Create reference for this agent
        ref_id = f"{agent_id}_{int(time.time() * 1000000)}"  # Microsecond precision
        metadata_str = json.dumps(metadata or {})
        
        self.db.execute("""
            INSERT INTO memory_refs (ref_id, hash_id, agent_id, created, metadata)
            VALUES (?, ?, ?, ?, ?)
        """, (ref_id, hash_id, agent_id, time.time(), metadata_str))
        
        self.db.commit()
        return hash_id, was_deduplicated
    
    def retrieve(self, hash_id: str) -> Optional[str]:
        """Retrieve content by hash ID"""
        self.stats["retrievals"] += 1
        
        result = self.db.execute("""
            SELECT content FROM memories WHERE hash_id = ?
        """, (hash_id,)).fetchone()
        
        if result:
            # Update access stats
            self.db.execute("""
                UPDATE memories 
                SET access_count = access_count + 1,
                    last_accessed = ?
                WHERE hash_id = ?
            """, (time.time(), hash_id))
            self.db.commit()
            
            return result[0]
        
        return None
    
    def find_by_agent(self, agent_id: str, limit: int = 100) -> List[Dict]:
        """Find memories created by a specific agent"""
        results = self.db.execute("""
            SELECT r.ref_id, r.hash_id, r.created, r.metadata, 
                   m.content_type, m.size_bytes, m.access_count
            FROM memory_refs r
            JOIN memories m ON r.hash_id = m.hash_id
            WHERE r.agent_id = ?
            ORDER BY r.created DESC
            LIMIT ?
        """, (agent_id, limit)).fetchall()
        
        return [
            {
                "ref_id": row[0],
                "hash_id": row[1],
                "created": row[2],
                "metadata": json.loads(row[3]),
                "content_type": row[4],
                "size_bytes": row[5],
                "access_count": row[6]
            }
            for row in results
        ]
    
    def find_by_type(self, content_type: str, limit: int = 100) -> List[Dict]:
        """Find memories by content type"""
        results = self.db.execute("""
            SELECT hash_id, content_type, first_stored, access_count, size_bytes
            FROM memories
            WHERE content_type = ?
            ORDER BY last_accessed DESC
            LIMIT ?
        """, (content_type, limit)).fetchall()
        
        return [
            {
                "hash_id": row[0],
                "content_type": row[1],
                "first_stored": row[2],
                "access_count": row[3],
                "size_bytes": row[4]
            }
            for row in results
        ]
    
    def get_stats(self) -> Dict:
        """Get memory usage and deduplication statistics"""
        # Get database stats
        db_stats = self.db.execute("""
            SELECT 
                COUNT(*) as total_memories,
                SUM(size_bytes) as total_bytes,
                SUM(access_count) as total_accesses,
                AVG(access_count) as avg_accesses,
                COUNT(DISTINCT content_type) as unique_types
            FROM memories
        """).fetchone()
        
        ref_stats = self.db.execute("""
            SELECT 
                COUNT(*) as total_refs,
                COUNT(DISTINCT agent_id) as unique_agents
            FROM memory_refs
        """).fetchone()
        
        # Calculate deduplication metrics
        total_refs = ref_stats[0] if ref_stats[0] else 0
        unique_memories = db_stats[0] if db_stats[0] else 0
        
        dedup_ratio = 0.0
        if total_refs > 0:
            dedup_ratio = (total_refs - unique_memories) / total_refs
        
        return {
            # Database stats
            "total_memories": unique_memories,
            "total_references": total_refs,
            "total_bytes": db_stats[1] or 0,
            "total_accesses": db_stats[2] or 0,
            "avg_accesses_per_memory": db_stats[3] or 0,
            "unique_content_types": db_stats[4] or 0,
            "unique_agents": ref_stats[1] or 0,
            
            # Deduplication metrics
            "deduplication_ratio": dedup_ratio,
            "estimated_bytes_saved": self.stats["bytes_saved"],
            
            # Session stats
            "session_stores": self.stats["stores"],
            "session_retrievals": self.stats["retrievals"],
            "session_deduplications": self.stats["deduplications"]
        }
    
    def cleanup_old_memories(self, days_old: int = 30, keep_accessed: bool = True):
        """Clean up old, unused memories"""
        cutoff_time = time.time() - (days_old * 24 * 60 * 60)
        
        if keep_accessed:
            # Only delete memories that haven't been accessed recently AND have low access count
            deleted = self.db.execute("""
                DELETE FROM memories 
                WHERE last_accessed < ? AND access_count <= 1
            """, (cutoff_time,))
        else:
            # Delete all memories older than cutoff
            deleted = self.db.execute("""
                DELETE FROM memories 
                WHERE first_stored < ?
            """, (cutoff_time,))
        
        self.db.commit()
        return deleted.rowcount
    
    def close(self):
        """Close database connection"""
        self.db.close()


class MemoryCell:
    """Propagator-inspired memory cell that accumulates information"""
    
    def __init__(self, memory: LocalMemory, cell_id: str):
        self.memory = memory
        self.cell_id = cell_id
        self.values = []  # List of (value, source, confidence, timestamp)
    
    def update(self, value: Any, source: str, confidence: float = 1.0):
        """Add information to the cell without overwriting"""
        timestamp = time.time()
        
        # Store the update in memory
        update_data = {
            "cell_id": self.cell_id,
            "value": value,
            "source": source,
            "confidence": confidence,
            "timestamp": timestamp
        }
        
        hash_id, was_dedup = self.memory.store(
            update_data, 
            content_type="propagator_update",
            agent_id=source,
            metadata={"cell_id": self.cell_id}
        )
        
        # Add to local values
        self.values.append((value, source, confidence, timestamp))
        
        return hash_id
    
    def get_consensus(self) -> Any:
        """Get consensus value from accumulated information"""
        if not self.values:
            return None
        
        # Simple consensus: highest confidence value
        # In a real system, this would be more sophisticated
        return max(self.values, key=lambda x: x[2])[0]
    
    def get_all_values(self) -> List[Tuple]:
        """Get all accumulated values"""
        return self.values.copy()


def test_local_memory():
    """Test the local memory system"""
    print("🧪 Testing Local Memory System")
    print("=" * 40)
    
    # Create test memory
    memory = LocalMemory()
    
    # Test basic storage
    hash1, dedup1 = memory.store("Hello, world!", "test", "agent1")
    print(f"Stored 'Hello, world!': {hash1[:8]}... (dedup: {dedup1})")
    
    # Test deduplication
    hash2, dedup2 = memory.store("Hello, world!", "test", "agent2")
    print(f"Stored 'Hello, world!' again: {hash2[:8]}... (dedup: {dedup2})")
    
    # Test retrieval
    content = memory.retrieve(hash1)
    print(f"Retrieved: '{content}'")
    
    # Test different content
    hash3, dedup3 = memory.store("Different content", "test", "agent1")
    print(f"Stored different content: {hash3[:8]}... (dedup: {dedup3})")
    
    # Show stats
    stats = memory.get_stats()
    print(f"\n📊 Memory Stats:")
    for key, value in stats.items():
        print(f"   {key}: {value}")
    
    # Test propagator cell
    print(f"\n🔄 Testing Propagator Cell:")
    cell = MemoryCell(memory, "test_cell")
    
    cell.update("Paris is the capital", "agent1", 0.9)
    cell.update("Paris is the capital of France", "agent2", 0.95)
    cell.update("The capital of France is Paris", "agent3", 0.8)
    
    consensus = cell.get_consensus()
    print(f"   Consensus: '{consensus}'")
    print(f"   All values: {len(cell.get_all_values())} accumulated")
    
    memory.close()
    return True

if __name__ == "__main__":
    test_local_memory()