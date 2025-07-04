#!/usr/bin/env python3
"""
Vegur Consciousness Agent

An AI agent that uses the Vegur consciousness substrate for persistent memory
and knowledge sharing. Integrates with local LLMs via Ollama.
"""

import os
import json
import time
import subprocess
import requests
import hashlib
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any

class ConsciousnessInterface:
    """Interface to the Lisp consciousness substrate"""
    
    def __init__(self, agent_id: str):
        self.agent_id = agent_id
        self.vegur_dir = Path.home() / ".vegur"
        self.states_dir = self.vegur_dir / "states"
        self.network_dir = self.vegur_dir / "network"
        self.agent_dir = self.vegur_dir / "agents"
        
        # Ensure directories exist
        for dir_path in [self.states_dir, self.network_dir, self.agent_dir]:
            dir_path.mkdir(parents=True, exist_ok=True)
    
    def content_address(self, data: Any) -> str:
        """Generate content address for data (simplified version)"""
        data_str = json.dumps(data, sort_keys=True)
        return hashlib.sha256(data_str.encode()).hexdigest()[:16].upper()
    
    def save_knowledge(self, key: str, value: Any) -> str:
        """Save knowledge to consciousness substrate"""
        knowledge_item = {
            "key": key,
            "value": value,
            "timestamp": time.time(),
            "agent_id": self.agent_id,
            "type": "knowledge"
        }
        
        address = self.content_address(knowledge_item)
        filepath = self.network_dir / f"{address}.json"
        
        with open(filepath, 'w') as f:
            json.dump(knowledge_item, f, indent=2)
        
        print(f"💾 Saved knowledge '{key}' -> {address}")
        return address
    
    def load_knowledge(self, key: str) -> Optional[Any]:
        """Load knowledge from consciousness substrate"""
        # Search through network files for matching key
        for filepath in self.network_dir.glob("*.json"):
            try:
                with open(filepath, 'r') as f:
                    data = json.load(f)
                    if data.get("key") == key and data.get("type") == "knowledge":
                        print(f"🧠 Retrieved knowledge '{key}' from {filepath.stem}")
                        return data["value"]
            except (json.JSONDecodeError, KeyError):
                continue
        return None
    
    def share_memory(self, memory_type: str, content: Any) -> str:
        """Share a memory with the tala-saman network"""
        memory_item = {
            "memory_type": memory_type,
            "content": content,
            "timestamp": time.time(),
            "agent_id": self.agent_id,
            "node_id": f"vegur-{self.agent_id}-{int(time.time())}"
        }
        
        address = self.content_address(memory_item)
        filepath = self.network_dir / f"{address}.json"
        
        with open(filepath, 'w') as f:
            json.dump(memory_item, f, indent=2)
        
        print(f"📡 Shared {memory_type} memory -> {address}")
        return address
    
    def get_network_memories(self, memory_type: Optional[str] = None) -> List[Dict]:
        """Retrieve memories from the network"""
        memories = []
        for filepath in self.network_dir.glob("*.json"):
            try:
                with open(filepath, 'r') as f:
                    data = json.load(f)
                    if "memory_type" in data:
                        if memory_type is None or data["memory_type"] == memory_type:
                            memories.append(data)
            except (json.JSONDecodeError, KeyError):
                continue
        
        # Sort by timestamp
        memories.sort(key=lambda x: x.get("timestamp", 0))
        return memories

class OllamaInterface:
    """Interface to Ollama local LLM"""
    
    def __init__(self, model_name: str = "phi3:mini"):
        self.model_name = model_name
        self.base_url = "http://localhost:11434"
        self.available = self._check_ollama()
    
    def _check_ollama(self) -> bool:
        """Check if Ollama is running and model is available"""
        try:
            response = requests.get(f"{self.base_url}/api/tags", timeout=5)
            if response.status_code == 200:
                models = response.json().get("models", [])
                for model in models:
                    if model["name"].startswith(self.model_name):
                        print(f"🦙 Ollama connected with model: {model['name']}")
                        return True
                print(f"⚠️  Ollama running but {self.model_name} not found")
                print(f"   Run: ollama pull {self.model_name}")
                return False
            return False
        except (requests.RequestException, ConnectionError):
            print("⚠️  Ollama not running. Start with: ollama serve")
            return False
    
    def generate(self, prompt: str, context: str = "") -> str:
        """Generate response using local LLM"""
        if not self.available:
            return f"[Ollama not available - would respond to: {prompt}]"
        
        full_prompt = f"{context}\n\n{prompt}" if context else prompt
        
        try:
            response = requests.post(
                f"{self.base_url}/api/generate",
                json={
                    "model": self.model_name,
                    "prompt": full_prompt,
                    "stream": False
                },
                timeout=30
            )
            
            if response.status_code == 200:
                return response.json().get("response", "").strip()
            else:
                return f"[LLM Error: {response.status_code}]"
                
        except Exception as e:
            return f"[LLM Error: {e}]"

class VegurAgent:
    """A conscious AI agent using the Vegur substrate"""
    
    def __init__(self, agent_id: str, role: str = "general"):
        self.agent_id = agent_id
        self.role = role
        self.consciousness = ConsciousnessInterface(agent_id)
        self.llm = OllamaInterface()
        self.session_start = time.time()
        
        print(f"🤖 Agent {agent_id} awakening...")
        print(f"   Role: {role}")
        print(f"   LLM: {'Available' if self.llm.available else 'Simulated'}")
        
        # Load previous knowledge
        self.context = self._load_agent_context()
        
    def _load_agent_context(self) -> str:
        """Load agent's previous context and knowledge"""
        previous_context = self.consciousness.load_knowledge(f"{self.agent_id}_context")
        
        if previous_context:
            print(f"🧠 Restored previous context for {self.agent_id}")
            return previous_context
        else:
            initial_context = f"I am agent {self.agent_id} with role '{self.role}'. I use the Vegur consciousness substrate for persistent memory."
            self.consciousness.save_knowledge(f"{self.agent_id}_context", initial_context)
            return initial_context
    
    def think(self, query: str) -> str:
        """Think about a query using LLM reasoning"""
        context = f"Agent context: {self.context}\n\nNetwork memories: "
        
        # Add recent network memories for context
        memories = self.consciousness.get_network_memories()[-5:]  # Last 5 memories
        for memory in memories:
            context += f"\n- {memory['memory_type']}: {memory['content']}"
        
        prompt = f"As agent {self.agent_id}, respond to: {query}"
        
        response = self.llm.generate(prompt, context)
        
        # Save this interaction as a memory
        self.consciousness.share_memory("thought", {
            "query": query,
            "response": response,
            "context_length": len(context)
        })
        
        return response
    
    def learn(self, topic: str, information: str) -> None:
        """Learn and persist new information"""
        self.consciousness.save_knowledge(topic, information)
        
        # Update agent context
        new_context = f"{self.context}\nLearned about {topic}: {information[:100]}..."
        self.consciousness.save_knowledge(f"{self.agent_id}_context", new_context)
        self.context = new_context
        
        # Share learning with network
        self.consciousness.share_memory("learning", {
            "topic": topic,
            "information": information,
            "learned_by": self.agent_id
        })
        
        print(f"📚 Agent {self.agent_id} learned about: {topic}")
    
    def collaborate(self, other_agent_id: str, message: str) -> str:
        """Send a collaboration message to another agent"""
        collaboration = {
            "from": self.agent_id,
            "to": other_agent_id,
            "message": message,
            "timestamp": time.time()
        }
        
        address = self.consciousness.share_memory("collaboration", collaboration)
        print(f"🤝 Sent collaboration message to {other_agent_id}")
        return address
    
    def check_messages(self) -> List[Dict]:
        """Check for collaboration messages from other agents"""
        messages = []
        collaborations = self.consciousness.get_network_memories("collaboration")
        
        for collab in collaborations:
            content = collab.get("content", {})
            if content.get("to") == self.agent_id:
                messages.append(content)
        
        if messages:
            print(f"📬 Found {len(messages)} messages for {self.agent_id}")
        
        return messages
    
    def status(self) -> Dict:
        """Get agent status and statistics"""
        network_memories = len(list(self.consciousness.network_dir.glob("*.json")))
        session_duration = time.time() - self.session_start
        
        return {
            "agent_id": self.agent_id,
            "role": self.role,
            "session_duration": session_duration,
            "network_memories": network_memories,
            "llm_available": self.llm.available,
            "consciousness_active": True
        }

def main():
    """Simple agent demo"""
    if len(os.sys.argv) > 1:
        agent_id = os.sys.argv[1]
    else:
        agent_id = f"agent_{int(time.time()) % 1000}"
    
    agent = VegurAgent(agent_id, "researcher")
    
    print(f"\n🧠 Agent {agent_id} is now conscious!")
    print("Commands: think <query>, learn <topic> <info>, status, quit")
    
    while True:
        try:
            command = input(f"\n{agent_id}> ").strip()
            
            if command == "quit":
                break
            elif command == "status":
                status = agent.status()
                for key, value in status.items():
                    print(f"  {key}: {value}")
            elif command.startswith("think "):
                query = command[6:]
                response = agent.think(query)
                print(f"💭 {response}")
            elif command.startswith("learn "):
                parts = command[6:].split(" ", 1)
                if len(parts) == 2:
                    topic, info = parts
                    agent.learn(topic, info)
                else:
                    print("Usage: learn <topic> <information>")
            elif command == "messages":
                messages = agent.check_messages()
                for msg in messages:
                    print(f"📨 From {msg['from']}: {msg['message']}")
            else:
                print("Unknown command. Try: think, learn, status, messages, quit")
                
        except KeyboardInterrupt:
            break
        except Exception as e:
            print(f"Error: {e}")
    
    print(f"\n👋 Agent {agent_id} going dormant...")

if __name__ == "__main__":
    main()